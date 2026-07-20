defmodule Exhub.Genclaw.Tools.Search do
  @moduledoc """
  search tool — web/image search via exhub's search MCP server.

  Delegates to the exhub `search` MCP server's web_search_exa tool.
  Returns facts and reference image paths in a GenClaw-compatible format.
  """

  require Logger

  alias Exhub.Genclaw.{Session, ToolCards}
  alias Exhub.MCP.Hub.BuiltInRegistry

  def build do
    descriptions = ToolCards.load_all_descriptions()
    desc = Map.get(descriptions, "search", default_description())

    LangChain.Function.new!(%{
      name: "search",
      description: desc,
      parameters_schema: %{
        "type" => "object",
        "properties" => %{
          "text_queries" => %{
            "type" => "array",
            "items" => %{"type" => "string"},
            "description" =>
              "Self-contained web-search queries to run, ONE BY ONE. Each must stand on its own."
          },
          "image_queries" => %{
            "type" => "array",
            "items" => %{"type" => "string"},
            "description" => "Image-search subjects to run, ONE BY ONE."
          },
          "need_process_problem" => %{
            "type" => "array",
            "items" => %{"type" => "string"},
            "description" => "Alias for text_queries. Merged with text_queries."
          },
          "final_count" => %{
            "type" => "integer",
            "default" => 1,
            "minimum" => 1,
            "maximum" => 8,
            "description" => "Max reference images to return PER image query."
          },
          "user_intent" => %{
            "type" => "string",
            "description" => "Original user prompt (used as image-rerank intent only)."
          }
        },
        "additionalProperties" => false
      },
      function: fn args, _ctx ->
        text_qs = dedup(Map.get(args, "text_queries", []) ++ Map.get(args, "need_process_problem", []))
        image_qs = dedup(Map.get(args, "image_queries", []))
        user_intent = Map.get(args, "user_intent", "")

        if text_qs == [] and image_qs == [] do
          Jason.encode!(%{"status" => "failed", "error" => "search: provide text_queries and/or image_queries"})
        else
          facts = run_text_searches(text_qs)
          reference_paths = run_image_searches(image_qs, user_intent)

          status = if reference_paths != [] or facts != "", do: "success", else: "failed"

          Jason.encode!(%{
            "reference_paths" => reference_paths,
            "text_queries" => text_qs,
            "image_queries" => image_qs,
            "enriched_prompt" => user_intent,
            "facts" => String.slice(facts, 0, 1800),
            "status" => status
          })
        end
      end
    })
  end

  defp run_text_searches(queries) do
    Enum.map(queries, fn q ->
      case BuiltInRegistry.call_tool("search", "web_search_exa", %{"query" => q, "numResults" => 3}) do
        {:ok, result} ->
          text = extract_search_text(result)
          "### #{q}\n#{text}"

        {:error, _} ->
          "### #{q}\n(search error)"
      end
    end)
    |> Enum.join("\n\n")
    |> String.trim()
  end

  defp run_image_searches(queries, _user_intent) do
    out_dir = Session.tool_output_dir("search")

    Enum.flat_map(queries, fn q ->
      case BuiltInRegistry.call_tool("search", "web_search_exa", %{"query" => q <> " image", "numResults" => 3}) do
        {:ok, result} ->
          urls = extract_image_urls(result)
          download_images(urls, out_dir, q)

        {:error, _} ->
          []
      end
    end)
  end

  defp download_images(urls, out_dir, query) do
    Enum.with_index(urls)
    |> Enum.flat_map(fn {url, idx} ->
      ext = Path.extname(url) |> String.downcase()
      ext = if ext in [".png", ".jpg", ".jpeg", ".webp"], do: ext, else: ".png"
      name = "ref_#{query |> String.replace(~r/[^a-zA-Z0-9]/, "_") |> String.slice(0, 20)}_#{idx}#{ext}"
      path = Path.join(out_dir, name)

      case download_file(url, path) do
        :ok -> [Path.expand(path)]
        _ -> []
      end
    end)
  end

  defp download_file(url, path) do
    case Req.get(url, receive_timeout: 30_000) do
      %Req.Response{status: 200, body: body} when is_binary(body) ->
        File.write(path, body)
      _ ->
        :error
    end
  rescue
    _ -> :error
  end

  defp extract_search_text(result) do
    case result do
      %{text: text} when is_binary(text) -> text
      %{"text" => text} when is_binary(text) -> text
      %{content: content} when is_binary(content) -> content
      _ -> inspect(result)
    end
  end

  defp extract_image_urls(result) do
    case result do
      %{highlights: highlights} when is_list(highlights) ->
        Enum.flat_map(highlights, &extract_urls_from_text/1)

      %{"highlights" => highlights} when is_list(highlights) ->
        Enum.flat_map(highlights, &extract_urls_from_text/1)

      %{content: content} when is_binary(content) ->
        extract_urls_from_text(content)

      _ ->
        []
    end
  end

  defp extract_urls_from_text(text) do
    Regex.scan(~r/https?:\/\/[^\s"'<>\]]+\.(?:png|jpg|jpeg|webp)/i, text)
    |> Enum.map(&List.first/1)
    |> Enum.filter(&(&1 != nil))
  end

  defp dedup(list) do
    list
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
  end

  defp default_description do
    "Search the web for facts and reference images. Returns verified facts and curated reference image paths."
  end
end
