defmodule Exhub.LspBridge.Config do
  @moduledoc """
  Loader for lsp-bridge's `langserver/*.json` and `multiserver/*.json`
  configuration tables.

  Port of the config lookup lsp-bridge does in `core/utils.py` /
  `lsp_bridge.py`. Each language server is described by a JSON file with the
  shape:

      {
        "name":         "elixirLS",
        "languageId":   "elixir",
        "command":      ["language_server.sh"],
        "projectFiles": ["mix.exs"],
        "settings":     { ... }
      }

  and each multi-server profile by a file whose *basename* is the profile name
  (e.g. `pyright_ruff.json`) and whose body maps a feature/method to an ordered
  list of server names, with an optional `default`:

      {
        "default":    "pyright",
        "completion": ["pyright", "ruff"],
        "formatting": ["ruff"]
      }

  Both directories are vendored under `priv/lsp_bridge/` and are read lazily
  and cached in an ETS table owned by this process, keyed by server name,
  language id and multi-server profile name. Call `reload/0` to re-read after
  editing the JSON on disk.

  ## Usage

      Exhub.LspBridge.Config.for_language("elixir")   # => {:ok, info} | {:error, :not_found}
      Exhub.LspBridge.Config.for_name("elixirLS")     # => {:ok, info} | {:error, :not_found}
      Exhub.LspBridge.Config.all()                    # => [%Config{}, ...]
      Exhub.LspBridge.Config.multi("pyright_ruff")    # => {:ok, %{...}} | {:error, :not_found}
  """

  use GenServer
  require Logger

  alias Exhub.LspBridge.MultiServer

  @table __MODULE__

  defstruct [
    :name,
    :language_id,
    :command,
    :args,
    :project_files,
    :settings,
    :support_single_file
  ]

  @type t :: %__MODULE__{}

  @doc "Full argv for the server: command + args (or nil when unconfigured)."
  @spec command_args(t()) :: [String.t()] | nil
  def command_args(%__MODULE__{command: nil}), do: nil
  def command_args(%__MODULE__{command: cmd, args: args}), do: [cmd | args]

  # ===========================================================================
  # Vendored config locations
  # ===========================================================================

  @doc "Directory holding the vendored `langserver/*.json` tables."
  def default_langserver_dir, do: priv_subdir("langserver")

  @doc "Directory holding the vendored `multiserver/*.json` profiles."
  def default_multiserver_dir, do: priv_subdir("multiserver")

  defp priv_subdir(sub) do
    priv =
      case :code.priv_dir(:exhub) do
        {:error, _} -> Path.join(File.cwd!(), "priv")
        dir -> List.to_string(dir)
      end

    Path.join([priv, "lsp_bridge", sub])
  end

  # ===========================================================================
  # Public API
  # ===========================================================================

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: Keyword.get(opts, :name, __MODULE__))
  end

  @doc "Lazily ensure the loader is running (survives hot reload)."
  @spec ensure_started(keyword() | nil) :: {:ok, pid() | nil} | {:error, term()}
  def ensure_started(opts \\ []) do
    # A custom-named instance may already own the (global, named) config table;
    # in that case there is nothing to start — the lookups read the table
    # directly.
    cond do
      :ets.whereis(@table) != :undefined ->
        {:ok, Process.whereis(__MODULE__)}

      pid = Process.whereis(__MODULE__) ->
        {:ok, pid}

      true ->
        case GenServer.start(__MODULE__, normalize_opts(opts), name: __MODULE__) do
          {:ok, pid} -> {:ok, pid}
          {:error, {:already_started, pid}} -> {:ok, pid}
          {:error, reason} -> {:error, reason}
        end
    end
  end

  @doc "Look up a server config by its `languageId` (e.g. \"elixir\")."
  def for_language(language_id) when is_binary(language_id) do
    lookup({:lang, language_id})
  end

  @doc "Look up a server config by its `name` (e.g. \"elixirLS\")."
  def for_name(name) when is_binary(name) do
    lookup({:name, name})
  end

  @doc "Look up a multi-server profile by name (e.g. \"pyright_ruff\")."
  def multi(name) when is_binary(name) do
    lookup({:multi, name})
  end

  @doc "Ordered server names to use for `method` in the multi-server profile `name`."
  @spec multi_servers(String.t(), String.t()) :: [String.t()]
  def multi_servers(name, method) when is_binary(name) and is_binary(method) do
    MultiServer.servers(name, method)
  end

  @doc "Every loaded server config."
  def all do
    ensure_started()
    entries({:name, :_})
  end

  @doc "Every loaded multi-server profile, keyed by name."
  def multi_all do
    ensure_started()

    @table
    |> safe_tab2list()
    |> Enum.flat_map(fn
      {{:multi, name}, profile} -> [{name, profile}]
      _ -> []
    end)
    |> Map.new()
  end

  @doc "Re-read the langserver and multiserver directories from disk."
  def reload(opts \\ []) do
    ensure_started(opts)
    GenServer.call(__MODULE__, {:reload, normalize_opts(opts)})
  end

  defp lookup(key) do
    ensure_started()

    case :ets.whereis(@table) do
      :undefined ->
        {:error, :not_found}

      _ ->
        case :ets.lookup(@table, key) do
          [{^key, info}] -> {:ok, info}
          [] -> {:error, :not_found}
        end
    end
  catch
    :error, :badarg -> {:error, :not_found}
  end

  defp entries({prefix, :_}) do
    @table
    |> safe_tab2list()
    |> Enum.flat_map(fn
      {{^prefix, _k}, info} -> [info]
      _ -> []
    end)
  end

  defp safe_tab2list(table) do
    :ets.tab2list(table)
  rescue
    ArgumentError -> []
  end

  defp normalize_opts(opts) when is_list(opts), do: opts
  defp normalize_opts(dir) when is_binary(dir), do: [dir: dir]
  defp normalize_opts(nil), do: []

  # ===========================================================================
  # GenServer callbacks
  # ===========================================================================

  @impl true
  def init(opts) do
    lang_dir = resolve_dir(opts, :dir, :lsp_bridge_langserver_dir, default_langserver_dir())

    multi_dir =
      resolve_dir(opts, :multi_dir, :lsp_bridge_multiserver_dir, default_multiserver_dir())

    # Reuse an existing table if another (custom-named) instance already owns
    # one — otherwise `:ets.new/2` would raise `badarg`.
    table =
      case :ets.whereis(@table) do
        :undefined -> :ets.new(@table, [:named_table, :set, :public, read_concurrency: true])
        tid -> tid
      end

    load_langserver(lang_dir)
    load_multiserver(multi_dir)

    {:ok, %{dir: lang_dir, multi_dir: multi_dir, table: table}}
  end

  @impl true
  def handle_call({:reload, opts}, _from, state) do
    new_lang = if dir = opts[:dir], do: expand(dir), else: state.dir
    new_multi = if dir = opts[:multi_dir], do: expand(dir), else: state.multi_dir

    :ets.delete_all_objects(state.table)
    count = load_langserver(new_lang) + load_multiserver(new_multi)
    {:reply, {:ok, count}, %{state | dir: new_lang, multi_dir: new_multi}}
  end

  defp resolve_dir(opts, key, app_key, default) do
    opts
    |> Keyword.get(key, Application.get_env(:exhub, app_key, default))
    |> expand()
  end

  # ===========================================================================
  # Loading
  # ===========================================================================

  defp load_langserver(dir) do
    for_each_json(dir, fn _name, json -> store_langserver(json) end)
  end

  defp load_multiserver(dir) do
    for_each_json(dir, fn name, json -> store_multiserver(name, json) end)
  end

  defp for_each_json(dir, fun) do
    case File.ls(dir) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".json"))
        |> Enum.reduce(0, fn file, acc ->
          path = Path.join(dir, file)

          with {:ok, raw} <- File.read(path),
               {:ok, json} <- Jason.decode(raw) do
            fun.(Path.basename(file, ".json"), json)
            acc + 1
          else
            _ ->
              Logger.debug("[LspBridge.Config] skipping #{path}")
              acc
          end
        end)

      {:error, reason} ->
        Logger.warning("[LspBridge.Config] cannot read dir #{dir}: #{inspect(reason)}")
        0
    end
  end

  defp store_langserver(json) do
    info = parse(json)

    if info.name do
      :ets.insert(@table, {{:name, info.name}, info})
    end

    if info.language_id do
      # First writer wins per language id, matching lsp-bridge's dict semantics.
      unless match?([{:lang, _, _}], :ets.lookup(@table, {:lang, info.language_id})) do
        :ets.insert(@table, {{:lang, info.language_id}, info})
      end
    end
  end

  # Multi-server profile files are keyed by filename (lsp-bridge semantics), so
  # the raw map is stored as-is; `MultiServer` interprets it.
  defp store_multiserver(name, json) when is_map(json) do
    :ets.insert(@table, {{:multi, name}, json})
  end

  @doc false
  def parse(json) when is_map(json) do
    command = List.wrap(json["command"] || [])
    {executable, args} = split_command(command)

    %__MODULE__{
      name: json["name"],
      language_id: json["languageId"],
      command: executable,
      args: args,
      project_files: json["projectFiles"] || [],
      settings: json["settings"] || %{},
      support_single_file: Map.get(json, "support-single-file", true)
    }
  end

  # lsp-bridge lists the whole argv under "command"; normalize into
  # {executable, args}.
  defp split_command([exe | rest]), do: {exe, rest}
  defp split_command([]), do: {nil, []}

  defp expand(path), do: Path.expand(path)
end
