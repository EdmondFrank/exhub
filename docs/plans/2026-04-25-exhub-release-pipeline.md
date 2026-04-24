# Exhub Release Pipeline Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 实现 `exhub_release` MCP 工具 + `exhub-release` Emacs 命令，完成 compile → release → graceful-restart 完整服务更新闭环。

**Architecture:** 新增 `Exhub.MCP.Tools.Exhub.Release` 工具，内部串行执行 `mix compile`、`mix release --overwrite`、`GracefulRestart.schedule_restart/2`，任一步骤失败则立即终止并返回结构化错误报告。Emacs 端通过 RPC 子进程调用 `Exhub.MCP.Tools.Exhub.Release.run/1`，结果写入 `*exhub-release*` buffer 并在 minibuffer 显示摘要。

**Tech Stack:** Elixir / Anubis MCP framework / Exile (subprocess) / Emacs Lisp (start-process + sentinel)

---

## Task 1: 实现 `Exhub.MCP.Tools.Exhub.Release` MCP 工具

**Files:**
- Create: `lib/exhub/mcp/tools/exhub/release.ex`

**Step 1: 创建工具文件**

```elixir
defmodule Exhub.MCP.Tools.Exhub.Release do
  @moduledoc """
  MCP Tool: exhub_release

  One-shot release pipeline: compile → build release → graceful restart.

  Runs the three steps in sequence and stops immediately on any failure.
  All subprocess output is written to Logger so it appears in the server
  log; the tool response contains a structured summary of each step.
  """

  require Logger

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "exhub_release"

  @impl true
  def description do
    """
    Run the full Exhub release pipeline in one shot:

      1. `mix compile`          — compile all changed modules
      2. `mix release --overwrite` — build a new OTP release on disk
      3. Graceful VM restart    — hot-swap into the new release

    The pipeline stops at the first failure and returns the output of
    every step that was attempted.  Set `restart_mode` to "none" to
    build the release without restarting (useful for dry-runs or CI).

    Parameters:
    - env:          MIX_ENV for compile + release steps (default "prod")
    - working_dir:  Project root directory (auto-detected from mix.exs)
    - timeout_ms:   Per-step timeout in ms (default 300_000 = 5 min)
    - restart_mode: "soft" (default) | "hard" | "none"
    - delay_ms:     Milliseconds before VM restart fires (default 3_000)
    """
  end

  schema do
    field(:env, :string,
      description: "MIX_ENV for compile and release steps",
      default: "prod"
    )

    field(:working_dir, :string,
      description: "Project root directory (defaults to auto-detected mix.exs location)"
    )

    field(:timeout_ms, :integer,
      description: "Per-step timeout in milliseconds",
      default: 300_000
    )

    field(:restart_mode, :string,
      description: "Restart mode after release: soft, hard, or none",
      default: "soft"
    )

    field(:delay_ms, :integer,
      description: "Milliseconds to wait before triggering the VM restart",
      default: 3_000
    )
  end

  @impl true
  def execute(params, frame) do
    env         = Map.get(params, :env, "prod")
    working_dir = Map.get(params, :working_dir)
    timeout_ms  = Map.get(params, :timeout_ms, 300_000)
    restart_mode_str = Map.get(params, :restart_mode, "soft")
    delay_ms    = Map.get(params, :delay_ms, 3_000)

    unless mix_available?() do
      resp =
        Response.tool()
        |> Response.error(
          "Mix is not available. The release pipeline only works when " <>
            "running from the source tree, not from a release build."
        )

      {:reply, resp, frame}
    else
      working_dir =
        if working_dir,
          do: Helpers.expand_path(working_dir),
          else: find_project_root()

      with :ok <- validate_project_dir(working_dir),
           {:ok, restart_mode} <- parse_restart_mode(restart_mode_str) do
        run_pipeline(env, working_dir, timeout_ms, restart_mode, delay_ms, frame)
      else
        {:error, reason} ->
          resp = Response.tool() |> Response.error(reason)
          {:reply, resp, frame}
      end
    end
  end

  # --------------------------------------------------------------------------
  # Pipeline
  # --------------------------------------------------------------------------

  defp run_pipeline(env, working_dir, timeout_ms, restart_mode, delay_ms, frame) do
    Logger.info("[Release] Starting release pipeline (env=#{env}, dir=#{working_dir})")

    with {:ok, compile_result}  <- run_step("compile", "MIX_ENV=#{env} mix compile", working_dir, timeout_ms),
         {:ok, release_result}  <- run_step("release", "MIX_ENV=#{env} mix release --overwrite", working_dir, timeout_ms) do

      restart_result =
        case restart_mode do
          :none ->
            Logger.info("[Release] Skipping restart (restart_mode=none)")
            %{"scheduled" => false, "mode" => "none", "message" => "Restart skipped."}

          mode ->
            Logger.info("[Release] Scheduling #{mode} restart in #{delay_ms}ms")
            {:ok, _pid} = Exhub.GracefulRestart.schedule_restart(mode, delay_ms)

            %{
              "scheduled" => true,
              "mode"      => Atom.to_string(mode),
              "delay_ms"  => delay_ms,
              "message"   => "#{mode} restart scheduled in #{delay_ms}ms."
            }
        end

      summary =
        if restart_mode == :none,
          do: "Release pipeline complete (no restart).",
          else: "Release pipeline complete. #{restart_mode} restart in #{delay_ms}ms."

      result = %{
        "compile" => compile_result,
        "release" => release_result,
        "restart" => restart_result,
        "summary" => summary
      }

      Logger.info("[Release] #{summary}")
      resp = Response.tool() |> Helpers.toon_response(result)
      {:reply, resp, frame}
    else
      {:error, step, step_result} ->
        summary = "Release pipeline failed at step: #{step}."
        Logger.error("[Release] #{summary}")

        result = Map.merge(step_result, %{"summary" => summary})
        resp   = Response.tool() |> Helpers.toon_response(result)
        {:reply, resp, frame}
    end
  end

  # --------------------------------------------------------------------------
  # Step runner
  # --------------------------------------------------------------------------

  defp run_step(step_name, command, working_dir, timeout_ms) do
    Logger.info("[Release] Step [#{step_name}]: #{command}")
    argv = Helpers.shell_command_args(command, login: true)
    opts = [stderr: :consume, cd: working_dir]

    task =
      Task.async(fn ->
        try do
          Exile.stream(argv, opts)
          |> Enum.reduce({"", "", nil}, fn
            {:stdout, data}, {out, err, code} -> {out <> data, err, code}
            {:stderr, data}, {out, err, code} -> {out, err <> data, code}
            {:exit, {:status, code}}, {out, err, _} -> {out, err, code}
            {:exit, :epipe}, {out, err, _} -> {out, err, 0}
            _, acc -> acc
          end)
        rescue
          e -> {:error, Exception.message(e)}
        end
      end)

    case Task.yield(task, timeout_ms) || Task.shutdown(task, :brutal_kill) do
      {:ok, {:error, message}} ->
        Logger.error("[Release] Step [#{step_name}] raised: #{message}")
        {:error, step_name, %{step_name => %{"error" => message}}}

      {:ok, {stdout, stderr, exit_code}} ->
        exit_code = exit_code || 0
        log_step_output(step_name, stdout, stderr, exit_code)

        result = %{
          "exit_code" => exit_code,
          "stdout"    => String.trim(stdout),
          "stderr"    => String.trim(stderr)
        }

        if exit_code == 0 do
          {:ok, result}
        else
          {:error, step_name, %{step_name => result}}
        end

      nil ->
        Logger.error("[Release] Step [#{step_name}] timed out after #{timeout_ms}ms")
        {:error, step_name, %{step_name => %{"error" => "timed out after #{timeout_ms}ms"}}}
    end
  end

  defp log_step_output(step, stdout, stderr, exit_code) do
    if stdout != "", do: Logger.info("[Release/#{step}] stdout:\n#{String.trim(stdout)}")
    if stderr != "", do: Logger.warning("[Release/#{step}] stderr:\n#{String.trim(stderr)}")
    Logger.info("[Release/#{step}] exit_code=#{exit_code}")
  end

  # --------------------------------------------------------------------------
  # Public helper — called by Emacs RPC
  # --------------------------------------------------------------------------

  @doc """
  Convenience entry-point for Emacs RPC calls.

  Runs the release pipeline with production defaults and returns a
  human-readable summary string suitable for display in the minibuffer.

      exhub rpc "Exhub.MCP.Tools.Exhub.Release.run()"
  """
  def run(opts \\ []) do
    env          = Keyword.get(opts, :env, "prod")
    working_dir  = Keyword.get(opts, :working_dir, find_project_root())
    timeout_ms   = Keyword.get(opts, :timeout_ms, 300_000)
    restart_mode = Keyword.get(opts, :restart_mode, :soft)
    delay_ms     = Keyword.get(opts, :delay_ms, 3_000)

    Logger.info("[Release] RPC run/1 invoked (env=#{env})")

    with :ok <- validate_project_dir(working_dir),
         {:ok, _compile} <- run_step("compile", "MIX_ENV=#{env} mix compile", working_dir, timeout_ms),
         {:ok, _release} <- run_step("release", "MIX_ENV=#{env} mix release --overwrite", working_dir, timeout_ms) do

      case restart_mode do
        :none ->
          "[Exhub] Release complete (no restart scheduled)."

        mode ->
          {:ok, _pid} = Exhub.GracefulRestart.schedule_restart(mode, delay_ms)
          "[Exhub] Release complete. #{mode} restart in #{delay_ms}ms."
      end
    else
      {:error, step, _result} ->
        "[Exhub] Release failed at step: #{step}. Check server log for details."

      {:error, reason} ->
        "[Exhub] Release aborted: #{reason}"
    end
  end

  # --------------------------------------------------------------------------
  # Helpers
  # --------------------------------------------------------------------------

  defp mix_available?, do: Code.ensure_loaded?(Mix)

  defp parse_restart_mode("soft"), do: {:ok, :soft}
  defp parse_restart_mode("hard"), do: {:ok, :hard}
  defp parse_restart_mode("none"), do: {:ok, :none}

  defp parse_restart_mode(other),
    do: {:error, "Unknown restart_mode: #{inspect(other)}. Valid values: \"soft\", \"hard\", \"none\"."}

  defp validate_project_dir(nil),
    do: {:error, "Could not locate a project root (no mix.exs found)."}

  defp validate_project_dir(dir) do
    cond do
      not File.dir?(dir) ->
        {:error, "Directory does not exist: #{dir}"}

      not File.exists?(Path.join(dir, "mix.exs")) ->
        {:error, "Not a Mix project (mix.exs not found in #{dir})."}

      true ->
        :ok
    end
  end

  defp find_project_root do
    cwd = File.cwd!()
    find_up(cwd, "mix.exs") || cwd
  end

  defp find_up(dir, filename) do
    if File.exists?(Path.join(dir, filename)) do
      dir
    else
      parent = Path.dirname(dir)
      if parent == dir, do: nil, else: find_up(parent, filename)
    end
  end
end
```

**Step 2: 确认文件存在**

```bash
ls lib/exhub/mcp/tools/exhub/release.ex
```

Expected: 文件存在，无报错。

**Step 3: Commit**

```bash
git add lib/exhub/mcp/tools/exhub/release.ex
git commit -m "feat(mcp): add exhub_release pipeline tool (compile→release→restart)"
```

---

## Task 2: 注册 `exhub_release` 到 `ExhubServer`

**Files:**
- Modify: `lib/exhub/mcp/exhub_server.ex`

**Step 1: 在 component 列表末尾添加新工具，并更新 moduledoc 表格**

在 `component Exhub.MCP.Tools.Exhub.GetVersion` 后追加：

```elixir
  component Exhub.MCP.Tools.Exhub.Release
```

moduledoc 工具表格新增一行：

```
  | `exhub_release`  | Full pipeline: compile → release → restart |
```

**Step 2: 验证编译**

```bash
mix compile 2>&1 | tail -5
```

Expected: `Compiling 1 file (.ex)` 或 `warning:` 行，无 `error:` 行。

**Step 3: Commit**

```bash
git add lib/exhub/mcp/exhub_server.ex
git commit -m "feat(mcp): register exhub_release component in ExhubServer"
```

---

## Task 3: 添加 `exhub-release` Emacs 命令

**Files:**
- Modify: `exhub.el`（在 `exhub-reload` 函数之后、`exhub-graceful-restart-delay` defcustom 之前插入）

**Step 1: 插入新函数**

```elisp
(defun exhub-release ()
  "Run the full Exhub release pipeline via RPC.

Calls `Exhub.MCP.Tools.Exhub.Release.run/0' in the running BEAM VM,
which executes the following steps in sequence:

  1. mix compile        (MIX_ENV=prod)
  2. mix release --overwrite
  3. GracefulRestart.schedule_restart(:soft, 3000)

Progress is written to the server log.  The result summary is
displayed in the minibuffer and written to the *exhub-release* buffer.

If the backend binary is not executable (e.g. first-time setup before
any release exists), falls back to running the pipeline directly via
`mix run' in a shell buffer."
  (interactive)
  (message "[Exhub] Starting release pipeline...")
  (if (file-executable-p exhub-backend-path)
      (let ((buffer (get-buffer-create "*exhub-release*")))
        (with-current-buffer buffer
          (erase-buffer))
        (let ((proc (start-process "exhub-release" buffer
                                   exhub-backend-path "rpc"
                                   "Exhub.MCP.Tools.Exhub.Release.run()")))
          (set-process-sentinel
           proc
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (with-current-buffer (process-buffer proc)
                 (message "%s" (string-trim (buffer-string)))))))))
    (message "[Exhub] Backend not executable; running pipeline via mix in shell")
    (let ((default-directory (file-name-directory exhub-backend-path)))
      (async-shell-command
       (format "MIX_ENV=%s mix compile && MIX_ENV=%s mix release --overwrite"
               exhub-mix-env exhub-mix-env)
       "*exhub-release*"))))
```

**Step 2: 字节编译检查**

```bash
emacs --batch \
  --eval "(add-to-list 'load-path \"/Users/edmondfrank/.emacs.d/elpa-29.4/websocket-20230809.305\")" \
  -f batch-byte-compile exhub.el 2>&1
```

Expected: exit 0，只有已有的 `defcustom group` 警告，无新 error。

**Step 3: 清理编译产物并 Commit**

```bash
rm -f exhub.elc
git add exhub.el
git commit -m "feat(emacs): add exhub-release command (RPC release pipeline)"
```

---

## Task 4: 验证 & 最终提交

**Step 1: 完整编译验证**

```bash
mix compile 2>&1 | grep -E "error:|warning:" | head -20
```

Expected: 无 `error:` 行。

**Step 2: 检查 ExhubServer 工具列表**

```bash
grep "component\|exhub_release" lib/exhub/mcp/exhub_server.ex
```

Expected:
```
  component Exhub.MCP.Tools.Exhub.Compile
  component Exhub.MCP.Tools.Exhub.HotReload
  component Exhub.MCP.Tools.Exhub.Restart
  component Exhub.MCP.Tools.Exhub.GetStatus
  component Exhub.MCP.Tools.Exhub.GetVersion
  component Exhub.MCP.Tools.Exhub.Release
```

**Step 3: 检查 Emacs 函数存在**

```bash
grep -n "exhub-release" exhub.el
```

Expected: `defun exhub-release` 行存在。

**Step 4: 最终 commit（如有遗漏文件）**

```bash
git status
git add -A
git commit -m "feat: exhub release pipeline — compile→release→restart via MCP + Emacs RPC"
```

---

## 完整流程示意

```
用户: M-x exhub-release
  │
  └─► start-process exhub rpc "Exhub.MCP.Tools.Exhub.Release.run()"
            │
            ├─[Step 1] mix compile (MIX_ENV=prod)
            │   ├─ 成功 → 继续
            │   └─ 失败 → 返回 "[Exhub] Release failed at step: compile."
            │
            ├─[Step 2] mix release --overwrite
            │   ├─ 成功 → 继续
            │   └─ 失败 → 返回 "[Exhub] Release failed at step: release."
            │
            └─[Step 3] GracefulRestart.schedule_restart(:soft, 3000)
                └─ 返回 "[Exhub] Release complete. soft restart in 3000ms."
                         │
                         └─► sentinel → message minibuffer
                                      → *exhub-release* buffer
```

## 参数对照表

| 参数 | MCP Tool | RPC `run/1` | Emacs `exhub-release` |
|------|----------|-------------|----------------------|
| env | `env` (default "prod") | `:env` | 固定 `exhub-mix-env` |
| working_dir | `working_dir` (auto) | `:working_dir` | 自动探测 |
| timeout_ms | `timeout_ms` (300_000) | `:timeout_ms` | 固定 300_000 |
| restart_mode | `restart_mode` ("soft") | `:restart_mode` | 固定 `:soft` |
| delay_ms | `delay_ms` (3_000) | `:delay_ms` | 固定 3_000 |
