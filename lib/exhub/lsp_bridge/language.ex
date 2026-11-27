defmodule Exhub.LspBridge.Language do
  @moduledoc """
  File path → LSP `languageId` mapping for callers that have no editor.

  Emacs knows a buffer's `major-mode` and passes the matching `language-id`
  with every command. A headless caller (the `lsp` MCP server, a script) only
  has a path, so this module answers the same question from the file
  extension (or well-known file name).

  `languageId` values are the ones the vendored `langserver/*.json` tables
  declare, so a result here always resolves through
  `Exhub.LspBridge.Config.for_language/1`.

  ## Preferred servers

  Several languages have more than one configured server (`python`:
  `pyright`, `basedpyright`, `jedi`, `ruff`, `pylsp`; `elixir`: `elixirLS`,
  `lexical`, `nextls`; `typescript`: `typescript`, `deno`). `Config` keeps the
  *first writer* per language id, and the order it sees files in is whatever
  `File.ls/1` returns — fine for Emacs, which pins a profile, but not a sane
  default for a headless call. `preferred/1` names the server a headless
  caller should try first; `candidates/1` returns it followed by every other
  configured server for that language, so a caller can fall back when the
  preferred binary is not installed.
  """

  alias Exhub.LspBridge.Config

  @extensions %{
    ".zig" => "zig",
    ".zon" => "zig",
    ".ex" => "elixir",
    ".exs" => "elixir",
    ".ts" => "typescript",
    ".mts" => "typescript",
    ".cts" => "typescript",
    ".tsx" => "typescriptreact",
    ".js" => "javascript",
    ".mjs" => "javascript",
    ".cjs" => "javascript",
    ".jsx" => "javascriptreact",
    ".py" => "python",
    ".pyi" => "python",
    ".rs" => "rust",
    ".go" => "go",
    ".lua" => "lua",
    ".rb" => "ruby",
    ".rake" => "ruby",
    ".php" => "php",
    ".java" => "java",
    ".kt" => "kotlin",
    ".kts" => "kotlin",
    ".scala" => "scala",
    ".c" => "c++",
    ".h" => "c++",
    ".cc" => "c++",
    ".cpp" => "c++",
    ".cxx" => "c++",
    ".hpp" => "c++",
    ".hh" => "c++",
    ".hxx" => "c++",
    ".cs" => "csharp",
    ".csx" => "csharp",
    ".fs" => "f#",
    ".fsi" => "f#",
    ".fsx" => "f#",
    ".swift" => "swift",
    ".dart" => "dart",
    ".hs" => "haskell",
    ".ml" => "ocaml",
    ".mli" => "ocaml",
    ".nix" => "nix",
    ".sh" => "bash",
    ".bash" => "bash",
    ".zsh" => "bash",
    ".fish" => "fish",
    ".json" => "json",
    ".jsonc" => "json",
    ".toml" => "toml",
    ".yaml" => "yaml",
    ".yml" => "yaml",
    ".md" => "markdown",
    ".markdown" => "markdown",
    ".html" => "html",
    ".htm" => "html",
    ".css" => "css",
    ".scss" => "css",
    ".vue" => "vue",
    ".svelte" => "svelte",
    ".astro" => "astro",
    ".tex" => "tex",
    ".graphql" => "graphql",
    ".gql" => "graphql",
    ".clj" => "clojure",
    ".cljs" => "clojure",
    ".cljc" => "clojure",
    ".edn" => "clojure",
    ".elm" => "elm",
    ".erl" => "erlang",
    ".hrl" => "erlang",
    ".glsl" => "glsl",
    ".vert" => "glsl",
    ".frag" => "glsl",
    ".cmake" => "cmake"
  }

  @filenames %{
    "Dockerfile" => "Dockerfiles",
    "CMakeLists.txt" => "cmake"
  }

  # Build markers used to root a language server when the file is not inside a
  # git repository (the `langserver/*.json` tables only carry `projectFiles`
  # for some languages, and Emacs usually supplies the root itself).
  @project_files %{
    "astro" => ["package.json"],
    "bash" => [],
    "c++" => ["compile_commands.json", "CMakeLists.txt"],
    "clojure" => ["deps.edn", "project.clj"],
    "cmake" => ["CMakeLists.txt"],
    "dart" => ["pubspec.yaml"],
    "elixir" => ["mix.exs"],
    "elm" => ["elm.json"],
    "go" => ["go.mod"],
    "haskell" => ["stack.yaml", "cabal.project"],
    "java" => ["pom.xml", "build.gradle", "build.gradle.kts", "settings.gradle"],
    "javascript" => ["package.json"],
    "javascriptreact" => ["package.json"],
    "json" => ["package.json"],
    "kotlin" => ["build.gradle.kts", "settings.gradle.kts"],
    "lua" => [".luarc.json", ".stylua.toml"],
    "markdown" => [".marksman.toml"],
    "nix" => ["flake.nix"],
    "ocaml" => ["dune-project"],
    "php" => ["composer.json"],
    "python" => ["pyproject.toml", "setup.py", "setup.cfg"],
    "ruby" => ["Gemfile"],
    "rust" => ["Cargo.toml"],
    "scala" => ["build.sbt"],
    "svelte" => ["package.json"],
    "swift" => ["Package.swift"],
    "toml" => ["Cargo.toml"],
    "typescript" => ["tsconfig.json", "package.json"],
    "typescriptreact" => ["tsconfig.json", "package.json"],
    "vue" => ["package.json"],
    "zig" => ["build.zig", "build.zig.zon"]
  }

  @preferred %{
    "c++" => "clangd",
    "elixir" => "elixirLS",
    "java" => "jdtls",
    "javascript" => "javascript",
    "javascriptreact" => "javascriptreact",
    "json" => "vscode-json-language-server",
    "lua" => "lua-lsp",
    "markdown" => "marksman",
    "python" => "pyright",
    "toml" => "taplo",
    "typescript" => "typescript",
    "typescriptreact" => "typescriptreact",
    "vue" => "volar",
    "yaml" => "yaml-language-server",
    "zig" => "zls"
  }

  @doc "The LSP `languageId` for `path`, or `nil` when the extension is unknown."
  @spec language_for(Path.t()) :: String.t() | nil
  def language_for(path) when is_binary(path) do
    Map.get(@filenames, Path.basename(path)) || Map.get(@extensions, extension(path))
  end

  @doc "Whether a language server is configured for `path`'s language."
  @spec supported?(Path.t()) :: boolean()
  def supported?(path), do: language_for(path) != nil

  @doc "Every file extension this module maps to a language."
  @spec extensions() :: [String.t()]
  def extensions, do: @extensions |> Map.keys() |> Enum.sort()

  @doc "Name of the server a headless caller should try first for `language_id`."
  @spec preferred(String.t()) :: String.t() | nil
  def preferred(language_id), do: Map.get(@preferred, language_id)

  @doc "Build markers that identify a project of `language_id`, for non-git trees."
  @spec project_files(String.t()) :: [String.t()]
  def project_files(language_id), do: Map.get(@project_files, language_id, [])

  @doc """
  Every configured server for `language_id`, preferred one first, skipping
  platform-suffixed variants (`jdtls_windows`) that cannot run here.
  """
  @spec candidates(String.t()) :: [Config.t()]
  def candidates(language_id) when is_binary(language_id) do
    servers =
      Config.all()
      |> Enum.filter(&(&1.language_id == language_id))
      |> Enum.reject(&platform_mismatch?/1)

    case preferred(language_id) do
      nil -> servers
      name -> Enum.sort_by(servers, fn info -> if info.name == name, do: 0, else: 1 end)
    end
  end

  def candidates(_language_id), do: []

  defp platform_mismatch?(%Config{name: name}) do
    case :os.type() do
      {:win32, _} -> String.ends_with?(name, "_darwin")
      _ -> String.ends_with?(name, "_windows")
    end
  end

  defp extension(path), do: path |> Path.extname() |> String.downcase()
end
