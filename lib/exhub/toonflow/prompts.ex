defmodule Exhub.Toonflow.Prompts do
  @moduledoc """
  Prompt template loading, rendering and per-project overrides for the Toonflow
  pipeline.

  Shipped templates live in `priv/toonflow/prompts/<name>.md` and use `{{key}}`
  placeholders, substituted by `render_string/2`.

  `resolve/4` is what the generation stages call: it renders the *effective*
  template — the project's latest `prompt_overrides` row for that name when one
  exists (and overrides are enabled in config), else the shipped file. Overrides
  are immutable and versioned like scripts, so `history/3` shows every revision
  and `revert/4` restores an earlier one as a new version.

  `render/2` (shipped templates only) is kept for callers without a project.
  """

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{Config, DB, Schema, Store}

  @doc "Directory holding the prompt templates."
  @spec dir() :: String.t()
  def dir, do: Path.join(:code.priv_dir(:exhub), "toonflow/prompts")

  @doc "Load a template by name (without the `.md` suffix)."
  @spec load(String.t()) :: {:ok, String.t()} | {:error, term()}
  def load(name) when is_binary(name), do: File.read(Path.join(dir(), name <> ".md"))

  @doc "Load and render a shipped template, substituting `{{key}}` placeholders."
  @spec render(String.t(), map()) :: {:ok, String.t()} | {:error, term()}
  def render(name, assigns \\ %{}) do
    with {:ok, template} <- load(name) do
      {:ok, render_string(template, assigns)}
    end
  end

  @doc """
  Render the effective template for a project: its latest override when one
  exists and overrides are enabled, else the shipped file. Never fails on a
  database problem — a broken override lookup falls back to the shipped file.
  """
  @spec resolve(String.t(), map(), String.t(), GenServer.server()) ::
          {:ok, String.t()} | {:error, term()}
  def resolve(name, assigns, project, server \\ Store) do
    with {:ok, template} <- effective_template(name, project, server) do
      {:ok, render_string(template, assigns)}
    end
  end

  @doc """
  The effective template text plus where it came from:
  `%{"name", "source" ("override" | "shipped"), "version", "content"}`.
  """
  @spec current(String.t(), String.t(), GenServer.server()) ::
          {:ok, map()} | {:error, term()}
  def current(name, project, server \\ Store) do
    case latest_override(name, project, server) do
      {:ok, %{"content" => content} = override} ->
        {:ok,
         %{
           "name" => name,
           "source" => "override",
           "version" => override["version"],
           "content" => content
         }}

      _ ->
        with {:ok, content} <- load(name) do
          {:ok, %{"name" => name, "source" => "shipped", "version" => nil, "content" => content}}
        end
    end
  end

  @doc """
  Save a new version of a project's prompt override.

  `opts`: `:name` (the template name, required), `:content` (required),
  `:note`. Returns `{:ok, override}` or `{:error, reason}`.
  """
  @spec set(String.t(), keyword(), GenServer.server()) :: {:ok, map()} | {:error, term()}
  def set(project, opts \\ [], server \\ Store) do
    name = Toonflow.blank(Keyword.get(opts, :name))
    content = Toonflow.blank(Keyword.get(opts, :content))
    note = Keyword.get(opts, :note)

    cond do
      is_nil(name) ->
        {:error, :missing_name}

      is_nil(content) ->
        {:error, :missing_content}

      true ->
        with {:ok, version} <- next_version(name, project, server) do
          insert(project, name, version, content, note, server)
        end
    end
  end

  @doc """
  Re-save the content of an earlier version as a new version (a revert that
  keeps the history). `opts`: `:name`, `:version`.
  """
  @spec revert(String.t(), keyword(), GenServer.server()) :: {:ok, map()} | {:error, term()}
  def revert(project, opts \\ [], server \\ Store) do
    name = Toonflow.blank(Keyword.get(opts, :name))
    version = Keyword.get(opts, :version)

    with {:ok, source} <- fetch(name, version, project, server) do
      set(
        project,
        [
          name: name,
          content: source["content"],
          note: "revert to v#{source["version"]}"
        ],
        server
      )
    end
  end

  @doc "Every version of a prompt override, newest first."
  @spec history(String.t(), String.t(), GenServer.server()) ::
          {:ok, [map()]} | {:error, term()}
  def history(name, project, server \\ Store) do
    name = Toonflow.blank(name)

    if is_nil(name) do
      {:error, :missing_name}
    else
      sql =
        "SELECT #{Schema.prompt_override_columns()} FROM prompt_overrides " <>
          "WHERE name = ? ORDER BY version DESC"

      Store.run_project(
        project,
        fn conn ->
          case DB.query(conn, sql, [name]) do
            {:ok, rows} -> {:ok, Enum.map(rows, &Schema.decode_prompt_override/1)}
            {:error, reason} -> {:error, reason}
          end
        end,
        server
      )
    end
  end

  @doc """
  The overridden templates in a project, newest version first, each with the
  version count and whether a shipped template of that name exists.
  """
  @spec list(String.t(), GenServer.server()) :: {:ok, [map()]} | {:error, term()}
  def list(project, server \\ Store) do
    sql =
      "SELECT #{Schema.prompt_override_columns()} FROM prompt_overrides " <>
        "ORDER BY name, version DESC"

    Store.run_project(
      project,
      fn conn ->
        case DB.query(conn, sql, []) do
          {:ok, rows} ->
            overrides = Enum.map(rows, &Schema.decode_prompt_override/1)

            {:ok, summarize(overrides)}

          {:error, reason} ->
            {:error, reason}
        end
      end,
      server
    )
  end

  @doc "Render a template string, substituting `{{key}}` placeholders."
  @spec render_string(String.t(), map()) :: String.t()
  def render_string(template, assigns) when is_binary(template) and is_map(assigns) do
    Enum.reduce(assigns, template, fn {key, value}, acc ->
      String.replace(acc, "{{#{key}}}", stringify(value))
    end)
  end

  @doc "Whether per-project overrides are honoured (config `prompts.overrides`)."
  @spec overrides_enabled?() :: boolean()
  def overrides_enabled?, do: (Config.get("prompts", %{}) || %{})["overrides"] != false

  # --- internals ---

  defp effective_template(name, project, server) do
    if overrides_enabled?() do
      case latest_override(name, project, server) do
        {:ok, %{"content" => content}} -> {:ok, content}
        _ -> load(name)
      end
    else
      load(name)
    end
  end

  defp latest_override(name, project, server) do
    sql =
      "SELECT #{Schema.prompt_override_columns()} FROM prompt_overrides " <>
        "WHERE name = ? ORDER BY version DESC LIMIT 1"

    Store.run_project(
      project,
      fn conn ->
        case DB.query_one(conn, sql, [name]) do
          {:ok, nil} -> {:ok, nil}
          {:ok, row} -> {:ok, Schema.decode_prompt_override(row)}
          {:error, reason} -> {:error, reason}
        end
      end,
      server
    )
  end

  defp fetch(name, version, project, server) do
    sql =
      "SELECT #{Schema.prompt_override_columns()} FROM prompt_overrides " <>
        "WHERE name = ? AND version = ? LIMIT 1"

    Store.run_project(
      project,
      fn conn ->
        case DB.query_one(conn, sql, [name, version]) do
          {:ok, nil} -> {:error, :version_not_found}
          {:ok, row} -> {:ok, Schema.decode_prompt_override(row)}
          {:error, reason} -> {:error, reason}
        end
      end,
      server
    )
  end

  defp next_version(name, project, server) do
    case history(name, project, server) do
      {:ok, []} -> {:ok, 1}
      {:ok, [latest | _]} -> {:ok, (latest["version"] || 0) + 1}
      {:error, reason} -> {:error, reason}
    end
  end

  defp insert(project, name, version, content, note, server) do
    id = Toonflow.new_id("prm")
    now = Toonflow.now_iso()

    result =
      Store.run_project(
        project,
        fn conn ->
          DB.execute(
            conn,
            """
            INSERT INTO prompt_overrides (id, name, version, content, note, created_at)
            VALUES (?, ?, ?, ?, ?, ?)
            """,
            [id, name, version, content, note, now]
          )
        end,
        server
      )

    case result do
      :ok ->
        {:ok,
         %{
           "id" => id,
           "name" => name,
           "version" => version,
           "content" => content,
           "note" => note,
           "created_at" => now
         }}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp summarize(overrides) do
    overrides
    |> Enum.group_by(& &1["name"])
    |> Enum.map(fn {name, versions} ->
      latest = Enum.max_by(versions, & &1["version"])

      %{
        "name" => name,
        "version" => latest["version"],
        "versions" => length(versions),
        "note" => latest["note"],
        "updated_at" => latest["created_at"],
        "shipped" => match?({:ok, _}, load(name))
      }
    end)
    |> Enum.sort_by(& &1["name"])
  end

  defp stringify(nil), do: ""
  defp stringify(value) when is_binary(value), do: value
  defp stringify(value), do: to_string(value)
end
