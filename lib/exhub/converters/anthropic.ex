defmodule Exhub.Converters.Anthropic do
  @moduledoc """
  Converts between Anthropic API format and LangChain format.

  This module handles the transformation of messages, content blocks,
  and tools between Anthropic's native API format and the LangChain
  message format used internally.
  """

  alias LangChain.Message
  alias LangChain.Message.ContentPart

  @typedoc "Anthropic message format"
  @type anthropic_message :: map()

  @typedoc "LangChain message format"
  @type langchain_message :: Message.t()

  @typedoc "Content block from Anthropic"
  @type content_block :: map()

  @doc """
  Converts an Anthropic API request to LangChain format.

  Transforms messages, system prompts, and tools from Anthropic's
  format to LangChain's expected format.

  ## Examples

      iex> anthropic_request = %{
      ...>   "messages" => [%{"role" => "user", "content" => "Hello"}],
      ...>   "system" => "You are helpful",
      ...>   "max_tokens" => 1000
      ...> }
      iex> Exhub.Converters.Anthropic.to_langchain(anthropic_request)
      %{messages: [%LangChain.Message{}], max_tokens: 1000, system: %LangChain.Message{}}
  """
  @spec to_langchain(map()) :: map()
  def to_langchain(anthropic_request) do
    messages = Map.get(anthropic_request, "messages", [])
    system = Map.get(anthropic_request, "system")
    tools = Map.get(anthropic_request, "tools", [])
    max_tokens = Map.get(anthropic_request, "max_tokens", 4096)

    langchain_messages = convert_messages(messages)

    result = %{
      messages: langchain_messages,
      max_tokens: max_tokens
    }

    result =
      if system do
        system_message = convert_system(system)
        if system_message, do: Map.put(result, :system, system_message), else: result
      else
        result
      end

    if tools != [] do
      langchain_tools = convert_tools(tools)
      Map.put(result, :tools, langchain_tools)
    else
      result
    end
  end

  @doc """
  Converts a system prompt to LangChain format.

  Handles both string and list (content blocks) formats.
  """
  @spec convert_system(String.t() | list() | any()) :: Message.t() | nil
  def convert_system(system) when is_binary(system) and system != "" do
    Message.new_system!(system)
  end

  def convert_system(list) when is_list(list) and list != [] do
    content = content_blocks_to_string(list)
    Message.new_system!(content)
  end

  def convert_system(_), do: nil

  @doc """
  Converts a list of Anthropic messages to LangChain messages.
  """
  @spec convert_messages(list(map())) :: list(Message.t())
  def convert_messages(messages) when is_list(messages) do
    messages
    |> Enum.map(&convert_message/1)
    |> Enum.reject(&is_nil/1)
  end

  def convert_messages(_), do: []

  @doc """
  Converts a single Anthropic message to LangChain format.
  """
  @spec convert_message(map()) :: Message.t() | nil
  def convert_message(msg) do
    role = Map.get(msg, "role")
    content = Map.get(msg, "content")

    case role do
      "user" -> convert_user_message(content)
      "assistant" -> convert_assistant_message(content)
      _ -> nil
    end
  end

  @spec convert_user_message(any()) :: Message.t() | nil
  defp convert_user_message(content) do
    case content do
      str when is_binary(str) ->
        Message.new_user!(str)

      list when is_list(list) ->
        parts = content_blocks_to_parts(list)
        Message.new_user!(parts)

      _ ->
        nil
    end
  end

  @spec convert_assistant_message(any()) :: Message.t() | nil
  defp convert_assistant_message(content) do
    case content do
      str when is_binary(str) ->
        Message.new_assistant!(str)

      list when is_list(list) ->
        parts = content_blocks_to_parts(list)
        Message.new_assistant!(parts)

      _ ->
        nil
    end
  end

  @doc """
  Converts content blocks to LangChain ContentPart structs.
  """
  @spec content_blocks_to_parts(list(map())) :: list(ContentPart.t() | map())
  def content_blocks_to_parts(blocks) when is_list(blocks) do
    blocks
    |> Enum.map(&convert_content_block/1)
    |> Enum.reject(&is_nil/1)
  end

  @spec convert_content_block(map()) :: ContentPart.t() | map() | nil
  defp convert_content_block(block) do
    case block do
      %{"type" => "text", "text" => text} when is_binary(text) ->
        ContentPart.text!(text)

      %{"type" => "text", "text" => text, "citations" => _citations} when is_binary(text) ->
        ContentPart.text!(text)

      %{"type" => "image", "source" => source} ->
        convert_image_source(source)

      %{"type" => "tool_use", "id" => id, "name" => name, "input" => input} ->
        %{
          type: :tool_use,
          id: id,
          name: name,
          input: input
        }

      %{"type" => "tool_result", "tool_use_id" => tool_use_id, "content" => content} ->
        %{
          type: :tool_result,
          tool_use_id: tool_use_id,
          content: convert_tool_result_content(content)
        }

      %{"type" => "thinking", "thinking" => thinking, "signature" => signature} ->
        %{
          type: :thinking,
          thinking: thinking,
          signature: signature
        }

      _ ->
        nil
    end
  end

  @doc """
  Converts content blocks to a plain string representation.
  """
  @spec content_blocks_to_string(list(map())) :: String.t()
  def content_blocks_to_string(blocks) when is_list(blocks) do
    Enum.map_join(blocks, fn block ->
      case block do
        %{"type" => "text", "text" => text} when is_binary(text) ->
          text

        %{"type" => "thinking", "thinking" => thinking} when is_binary(thinking) ->
          "[Thinking]\n#{thinking}\n[/Thinking]"

        _ ->
          ""
      end
    end)
  end

  def content_blocks_to_string(_), do: ""

  @spec convert_image_source(map()) :: ContentPart.t() | nil
  defp convert_image_source(source) do
    case source do
      %{"type" => "base64", "data" => data, "media_type" => media_type}
      when is_binary(data) and is_binary(media_type) ->
        media = parse_media_type(media_type)
        ContentPart.image!(data, media: media)

      %{"type" => "url", "url" => _url} ->
        raise "Anthropic doesn't support image_url directly"

      _ ->
        nil
    end
  end

  @spec parse_media_type(String.t()) :: atom() | String.t()
  defp parse_media_type("image/jpeg"), do: :jpeg
  defp parse_media_type("image/png"), do: :png
  defp parse_media_type("image/gif"), do: :gif
  defp parse_media_type("image/webp"), do: :webp
  defp parse_media_type(media_type) when is_binary(media_type), do: media_type
  defp parse_media_type(_), do: :png

  @spec convert_tool_result_content(any()) :: String.t()
  defp convert_tool_result_content(content) do
    case content do
      str when is_binary(str) -> str
      list when is_list(list) -> content_blocks_to_string(list)
      _ -> to_string(content)
    end
  end

  @doc """
  Converts Anthropic tools to LangChain Function format.

  Currently returns empty list as tool conversion is not fully implemented.
  """
  @spec convert_tools(list(map())) :: list()
  def convert_tools(tools) when is_list(tools) do
    # Tool conversion to LangChain.Function is planned but not yet implemented
    # as it requires schema transformation
    _ = tools
    []
  end

  def convert_tools(_), do: []
end
