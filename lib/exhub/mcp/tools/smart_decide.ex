defmodule Exhub.MCP.Tools.SmartDecide do
  @moduledoc """
  MCP Tool for System One structured decision making via Gitee AI / moark.com.

  Evaluates a `state` (the content to judge) against a flat set of typed
  `questions` and returns one structured answer per question. Backed by the
  OpenAI-style `POST /v1/systemone` endpoint, compatible with the TypeSafe
  System One / Jev contract; served by the `APUS-OpenJev-v1-9B` model (8k-token
  context) with `Bespoke-Nimble-9B` (2k context) still available.

  No free-form text is generated: the model scores the allowed answer tokens
  directly and returns the chosen answer plus calibrated probabilities.

  Question types:

    * `noul` — a yes/no judgment; returns the probability the answer is yes.
    * `choice` — picks one option from a set you define; returns the chosen
      option and the full probability distribution.
    * `score` — rates along an ordered rubric; returns the probability-weighted
      value across the levels.
  """

  alias Anubis.Server.Response

  use Anubis.Server.Component, type: :tool

  @api_url "https://api.moark.com/v1/systemone"
  @default_model "APUS-OpenJev-v1-9B"
  @request_timeout 120_000
  @valid_types ~w(noul choice score)

  def name, do: "smart_decide"

  @impl true
  def description do
    """
    Make fast, typed decisions about a piece of content ("state") using the
    System One decision model (APUS-OpenJev-v1-9B) via Gitee AI.

    This tool generates no reasoning or free-form text: it scores the allowed
    answer tokens directly and returns the chosen answer with its calibrated
    probability. Use it for routing, policy checks, yes/no judgments, and
    rubric scoring.

    Every question requires a non-empty `instructions` description.

    **Question types** (`questions.<id>.type`):
    - `noul` — yes/no judgment; returns the probability the answer is yes (0–1).
    - `choice` — pick one option from a set you define; `criteria` maps each
      option to its rubric description, or a list of option strings. Returns the
      chosen option plus the full probability distribution.
    - `score` — rate along an ordered rubric; `criteria` is an ordered list of
      levels (at least 2). Returns the probability-weighted score plus legend.

    **Returns:** `{ "model": ..., "answers": { "<id>": { ... } } }`, one answer
    per question. Set `compact: true` to drop the probability/confidence detail.
    """
  end

  schema do
    field(:state, :any,
      description:
        "The content to evaluate — plain text (string) or structured data (object/array). JSON strings are decoded automatically.",
      required: true
    )

    field(:questions, :any,
      description:
        "Map of question-id => { type, instructions, criteria }. Types: noul, choice, score. JSON strings are decoded automatically.",
      required: true
    )

    field(:model, :string,
      description:
        "System One model. Default: APUS-OpenJev-v1-9B (8k context); Bespoke-Nimble-9B (2k context) also available"
    )

    field(:compact, :boolean,
      description:
        "If true, return only the chosen values (drops probabilities/confidence/legend). Default: false"
    )
  end

  @impl true
  def execute(params, frame) do
    state = Map.get(params, :state) |> normalize_state()
    questions = Map.get(params, :questions)
    model = (Map.get(params, :model) || @default_model) |> to_string() |> String.trim()
    compact = Map.get(params, :compact, false) == true

    cond do
      is_nil(state) ->
        error(frame, "`state` is required — provide the text or structured data to evaluate")

      state == "" ->
        error(frame, "`state` must not be empty")

      is_nil(questions) ->
        error(frame, "`questions` is required — provide a non-empty map of typed questions")

      model == "" ->
        error(frame, "`model` must not be empty")

      true ->
        with {:ok, questions} <- normalize_questions(questions) do
          case decide(state, questions, model: model, compact: compact) do
            {:ok, result} ->
              resp = Response.tool() |> Response.json(result)
              {:reply, resp, frame}

            {:error, reason} ->
              error(frame, reason)
          end
        else
          {:error, reason} -> error(frame, reason)
        end
    end
  end

  @doc """
  Runs a System One decision without the MCP tool frame.

  `questions` must already be normalized (see `normalize_questions/1`) or be a
  plain map of question-id => `{ type, instructions, criteria }`. Returns
  `{:ok, %{"model" => model, "answers" => answers}}` or `{:error, message}`.

  Options:

    * `:model` — System One model id (default `#{@default_model}`)
    * `:compact` — drop probabilities/confidence/legend (default `false`)
    * `:api_key` — override the Gitee AI key configured at
      `:exhub, :giteeai_api_key`

  This is the programmatic entry point used by callers such as
  `Exhub.MCP.Hub.ToolRelevance` that need a decision outside MCP.
  """
  @spec decide(term(), term(), keyword()) :: {:ok, map()} | {:error, String.t()}
  def decide(state, questions, opts \\ []) do
    state = normalize_state(state)
    model = opts |> Keyword.get(:model, @default_model) |> to_string() |> String.trim()
    compact = Keyword.get(opts, :compact, false) == true
    api_key = Keyword.get(opts, :api_key) || Application.get_env(:exhub, :giteeai_api_key, "")

    cond do
      is_nil(state) ->
        {:error, "`state` is required — provide the text or structured data to evaluate"}

      state == "" ->
        {:error, "`state` must not be empty"}

      is_nil(questions) ->
        {:error, "`questions` is required — provide a non-empty map of typed questions"}

      model == "" ->
        {:error, "`model` must not be empty"}

      api_key == "" ->
        {:error,
         "Gitee AI API key not configured. Run: mix scr.insert dev giteeai_api_key \"your-key\""}

      true ->
        request(state, questions, model, compact, api_key)
    end
  end

  # --- Normalization helpers (pure) ---

  @doc """
  Decodes a JSON object/array string into a map/list; leaves plain text as-is.

  Only strings that look like a JSON object or array are decoded, so a plain
  text state such as `"123"` or `"true"` is preserved as text.
  """
  @spec normalize_state(term()) :: term()
  def normalize_state(value) when is_binary(value) do
    trimmed = String.trim(value)

    if String.starts_with?(trimmed, ["{", "["]) do
      case Jason.decode(trimmed) do
        {:ok, decoded} when is_map(decoded) or is_list(decoded) -> decoded
        _ -> value
      end
    else
      value
    end
  end

  def normalize_state(value), do: value

  @doc """
  Validates and normalizes the `questions` map.

  Returns `{:ok, questions}` with string keys, or `{:error, message}`. Accepts
  a JSON string for convenience. A `choice` question may express `criteria` as a
  list of option strings (Nimble-style), which is expanded to a map of
  option => nil.
  """
  @spec normalize_questions(term()) :: {:ok, map()} | {:error, String.t()}
  def normalize_questions(value) when is_binary(value) do
    case Jason.decode(value) do
      {:ok, decoded} -> normalize_questions(decoded)
      {:error, _} -> {:error, "`questions` must be a map — invalid JSON string"}
    end
  end

  def normalize_questions(questions) when is_map(questions) and map_size(questions) > 0 do
    questions
    |> Enum.reduce_while({:ok, %{}}, fn {id, question}, {:ok, acc} ->
      case normalize_question(id, question) do
        {:ok, normalized} -> {:cont, {:ok, Map.put(acc, to_string(id), normalized)}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  def normalize_questions(_),
    do: {:error, "`questions` must be a non-empty map of question-id => question"}

  @doc """
  Trims each answer to its chosen value (`noul`, `choice`, or `score`),
  dropping `probabilities`, `confidence`, and `legend`.
  """
  @spec compact_answers(term()) :: term()
  def compact_answers(answers) when is_map(answers) do
    Map.new(answers, fn {id, answer} -> {id, compact_answer(answer)} end)
  end

  def compact_answers(answers), do: answers

  defp compact_answer(answer) when is_map(answer) do
    case answer["type"] do
      "noul" -> %{"type" => "noul", "noul" => answer["noul"]}
      "choice" -> %{"type" => "choice", "choice" => answer["choice"]}
      "score" -> %{"type" => "score", "score" => answer["score"]}
      _ -> answer
    end
  end

  defp compact_answer(answer), do: answer

  defp normalize_question(id, question) when is_map(question) do
    type = question |> get(:type) |> stringify()
    instructions = question |> get(:instructions) |> stringify()
    criteria = get(question, :criteria)

    with :ok <- validate_type(id, type),
         :ok <- validate_instructions(id, instructions),
         {:ok, criteria} <- normalize_criteria(id, type, criteria) do
      normalized =
        %{"type" => type, "instructions" => instructions}
        |> maybe_put("criteria", criteria)

      {:ok, normalized}
    end
  end

  defp normalize_question(id, _question),
    do: {:error, "question `#{id}` must be an object with a `type` field"}

  defp validate_type(_id, type) when type in @valid_types, do: :ok

  defp validate_type(id, type) do
    {:error,
     "question `#{id}` has invalid type #{inspect(type)}; expected one of #{Enum.join(@valid_types, ", ")}"}
  end

  # The API rejects a missing or blank description ("a nonempty description is
  # required"), so validate locally for a clearer message.
  defp validate_instructions(id, instructions) when is_binary(instructions) do
    if String.trim(instructions) == "" do
      {:error, "question `#{id}` requires a non-empty `instructions` description"}
    else
      :ok
    end
  end

  defp validate_instructions(id, _instructions) do
    {:error, "question `#{id}` requires a non-empty `instructions` description"}
  end

  defp normalize_criteria(_id, "noul", nil), do: {:ok, nil}
  defp normalize_criteria(_id, "noul", criteria) when is_map(criteria), do: {:ok, criteria}

  defp normalize_criteria(id, "noul", _criteria) do
    {:error,
     "question `#{id}` (noul) criteria must be an object mapping \"true\"/\"false\" to descriptions"}
  end

  defp normalize_criteria(id, "choice", criteria) when is_map(criteria) do
    criteria
    |> normalize_choice_options()
    |> build_choice_criteria(id)
  end

  defp normalize_criteria(id, "choice", criteria) when is_list(criteria) do
    if criteria != [] and Enum.all?(criteria, &is_binary/1) do
      # ["a", "b"] expands to %{"a" => "a", "b" => "b"}: the option doubles as
      # its own description, matching the Nimble flat-schema convention.
      criteria
      |> Map.new(&{&1, &1})
      |> build_choice_criteria(id)
    else
      {:error,
       "question `#{id}` (choice) criteria must be an object or a non-empty list of option strings"}
    end
  end

  defp normalize_criteria(id, "choice", _criteria) do
    {:error,
     "question `#{id}` (choice) requires criteria: an object mapping each option to its description, or a list of options"}
  end

  defp normalize_criteria(id, "score", criteria) when is_list(criteria) do
    if length(criteria) >= 2 do
      {:ok, criteria}
    else
      {:error, "question `#{id}` (score) criteria must be an ordered list with at least 2 levels"}
    end
  end

  defp normalize_criteria(id, "score", _criteria) do
    {:error, "question `#{id}` (score) requires criteria: an ordered list of levels (at least 2)"}
  end

  # The API criteria dictionary requires non-empty string values, so a blank or
  # omitted description falls back to the option name itself.
  defp normalize_choice_options(criteria) do
    Map.new(criteria, fn {option, description} ->
      option = to_string(option)

      description =
        if is_binary(description) and String.trim(description) != "",
          do: description,
          else: option

      {option, description}
    end)
  end

  defp build_choice_criteria(options, id) do
    if map_size(options) >= 2 do
      {:ok, options}
    else
      {:error, "question `#{id}` (choice) requires at least 2 options in criteria"}
    end
  end

  defp get(map, key), do: Map.get(map, key) || Map.get(map, to_string(key))

  defp stringify(nil), do: nil
  defp stringify(value) when is_binary(value), do: String.trim(value)
  defp stringify(value), do: to_string(value)

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)

  # --- HTTP ---

  defp request(state, questions, model, compact, api_key) do
    body = %{"model" => model, "state" => state, "questions" => questions}

    headers = [
      {"Content-Type", "application/json"},
      {"Authorization", "Bearer #{api_key}"},
      {"X-Failover-Enabled", "true"}
    ]

    case HTTPoison.post(
           @api_url,
           Jason.encode!(body),
           headers,
           [recv_timeout: @request_timeout, timeout: @request_timeout] ++
             Exhub.TLSCompat.httpoison_opts(@api_url)
         ) do
      {:ok, %HTTPoison.Response{status_code: 200, body: resp_body}} ->
        decode_response(resp_body, model, compact)

      {:ok, %HTTPoison.Response{status_code: status, body: resp_body}} ->
        {:error, "Gitee AI System One API error (HTTP #{status}): #{resp_body}"}

      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, "HTTP request failed: #{inspect(reason)}"}
    end
  end

  defp decode_response(resp_body, model, compact) do
    case Jason.decode(resp_body) do
      {:ok, payload} when is_map(payload) ->
        answers = Map.get(payload, "answers", %{})
        answers = if compact, do: compact_answers(answers), else: answers
        {:ok, %{"model" => Map.get(payload, "model", model), "answers" => answers}}

      _ ->
        {:error, "Failed to decode System One response"}
    end
  end

  defp error(frame, message) do
    resp = Response.tool() |> Response.error(message)
    {:reply, resp, frame}
  end
end
