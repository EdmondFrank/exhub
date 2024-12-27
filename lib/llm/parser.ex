defmodule Exhub.Llm.Parser do
  use Ecto.Schema
  use Instructor

  alias LangChain.Message
  alias LangChain.Chains.LLMChain
  alias LangChain.Utils.ChainResult
  alias LangChain.ChatModels.ChatOpenAI
  alias Instructor.JSONSchema
  alias Exhub.Llm.Parser

  require Logger

  @config Application.compile_env(:exhub, :llm, %{model: "Qwen/Qwen2.5-7B-Instruct"})

  @llm_doc """
  ## Field Descriptions:
  - class_definitions: An array of maps, each containing details about a class, including its name, summary.
  - function_method_definitions: An array of maps, each containing details about a function or method, including its name, parameters, return type, and summary.
  - interface_definitions: An array of maps, each containing details about an interface, including its name, parameters, and return type.
  - module_definitions: An array of maps, each containing details about a module, including its name, summary.
  - class_references: An array of maps, each containing details about a reference to a class, including its name.
  - interface_implementations: An array of maps, each containing details about an implementation of an interface, including its name, parameters, return type, and summary.
  """
  @primary_key false
  embedded_schema do
    embeds_many :class_definitions, Class, primary_key: false do
      field :name, :string
      field :summary,  :string
    end
    embeds_many :function_method_definitions, Function, primary_key: false do
      field :name, :string
      field :params,  {:array, :string}
      field :return_type, :string
      field :summary,  :string
    end
    embeds_many :interface_definitions, Interface, primary_key: false do
      field :name, :string
      field :params,  {:array, :string}
      field :return_type, :string
    end
    embeds_many :module_definitions, Module, primary_key: false do
      field :name, :string
      field :summary,  :string
    end
    embeds_many :class_references, Reference, primary_key: false do
      field :name, :string
    end
    embeds_many :interface_implementations, Implementation, primary_key: false do
      field :name, :string
      field :params,  {:array, :string}
      field :return_type, :string
      field :summary,  :string
    end
  end

  @impl true
  def validate_changeset(changeset) do
    Instructor.cast_all(%Exhub.Llm.Parser{}, changeset)
  end

  def parse(path, max_retries \\ 3) do
    code = File.read!(path)

    llm_chain = %{
      llm: ChatOpenAI.new!(%{endpoint: "#{@config[:api_base]}/chat/completions", model: @config[:model]}),
      verbose: false
    }

    initial_messages = [
      Message.new_system!("""
      As a genius expert, your task is to understand the content and provide the parsed objects in json that match the following json_schema:
      ```
      #{JSONSchema.from_ecto_schema(Parser)}
      ```
      Note to ensure you do not return any irrelevant content, only return a JSON object.
      """),
      Message.new_user!(code)
    ]

    retry_parse(llm_chain, initial_messages, max_retries)
  end

  defp retry_parse(llm_chain, initial_messages, retries) when retries > 0 do
    {:ok, updated_chain} =
      LLMChain.new!(llm_chain)
      |> LLMChain.add_messages(initial_messages)
      |> LLMChain.run(mode: :while_needs_response)

    reply = updated_chain |> ChainResult.to_string() |> elem(1)

    try do
      # Extract the JSON block between ```json and ```
      cleaned_reply =
        case Regex.run(~r/\`\`\`json\n(.*?)\n\`\`\`/s, reply, capture: :first) do
          [json_block] -> String.trim(json_block, "```json\n") |> String.trim("\n```")
          nil -> reply
        end

      decoded_reply = Jason.decode!(cleaned_reply, keys: :atoms)
      changeset = validate_changeset(decoded_reply)

      if changeset.valid? do
        struct(Parser, decoded_reply)
      else
        Logger.debug("Validation failed. Retrying... (#{retries - 1} retries left)")
        retry_parse(llm_chain, initial_messages, retries - 1)
      end
    rescue
      e in Jason.DecodeError ->
        Logger.debug("JSON decoding failed: #{inspect(e)}. Retrying... (#{retries - 1} retries left)")
        retry_parse(llm_chain, initial_messages, retries - 1)
      e in RuntimeError ->
        Logger.debug("JSON block extraction failed: #{inspect(e)}. Retrying... (#{retries - 1} retries left)")
        retry_parse(llm_chain, initial_messages, retries - 1)
    end
  end

  defp retry_parse(_llm_chain, _initial_messages, 0) do
    raise "Max retries reached. Parsing failed."
  end
end
