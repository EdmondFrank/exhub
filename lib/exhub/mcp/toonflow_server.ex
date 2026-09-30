defmodule Exhub.MCP.ToonflowServer do
  @moduledoc """
  MCP server for the ExHub Toonflow subsystem — an AI short-drama pipeline
  (novel → script → storyboard → image → video → export) implemented natively on
  ExHub rather than as a port of the Node/TypeScript Toonflow app.

  Tools:

    * `toonflow_list_projects` / `toonflow_create_project` / `toonflow_project_info`
      — project management
    * `toonflow_add_novel` / `toonflow_list_chapters` — novel ingest & chaptering
    * `toonflow_extract_events` / `toonflow_list_events` — chapter event graph
    * `toonflow_generate_script` / `toonflow_get_script` / `toonflow_update_script`
      — script generation & versioning
    * `toonflow_extract_assets` / `toonflow_list_characters` — cast/scene/prop DB
    * `toonflow_generate_storyboard` / `toonflow_list_shots` — shot planning
    * `toonflow_generate_image` — frame generation
    * `toonflow_generate_video` / `toonflow_generate_voice` — clip & voice generation
    * `toonflow_assemble` / `toonflow_export` — FFmpeg episode assembly & packaging
    * `toonflow_pipeline_run` — orchestrate the whole pipeline (staged run/resume)
    * `toonflow_memory_add` / `toonflow_memory_list` / `toonflow_memory_search` /
      `toonflow_memory_index` — project memory & semantic recall
    * `toonflow_chat` / `toonflow_list_agents` — director agent chat

  Endpoint: `/toonflow/mcp` (built-in hub name `toonflow`).
  See `docs/modules/toonflow.md` and `docs/plans/2026-09-30-toonflow-design.md`.
  """

  use Anubis.Server,
    name: "exhub-toonflow-server",
    version: "0.1.0",
    capabilities: [:tools]

  component(Exhub.MCP.Tools.Toonflow.ListProjects)
  component(Exhub.MCP.Tools.Toonflow.CreateProject)
  component(Exhub.MCP.Tools.Toonflow.ProjectInfo)
  component(Exhub.MCP.Tools.Toonflow.AddNovel)
  component(Exhub.MCP.Tools.Toonflow.ListChapters)
  component(Exhub.MCP.Tools.Toonflow.ExtractEvents)
  component(Exhub.MCP.Tools.Toonflow.ListEvents)
  component(Exhub.MCP.Tools.Toonflow.GenerateScript)
  component(Exhub.MCP.Tools.Toonflow.GetScript)
  component(Exhub.MCP.Tools.Toonflow.UpdateScript)
  component(Exhub.MCP.Tools.Toonflow.ExtractAssets)
  component(Exhub.MCP.Tools.Toonflow.ListCharacters)
  component(Exhub.MCP.Tools.Toonflow.GenerateStoryboard)
  component(Exhub.MCP.Tools.Toonflow.ListShots)
  component(Exhub.MCP.Tools.Toonflow.GenerateImage)
  component(Exhub.MCP.Tools.Toonflow.GenerateVideo)
  component(Exhub.MCP.Tools.Toonflow.GenerateVoice)
  component(Exhub.MCP.Tools.Toonflow.Assemble)
  component(Exhub.MCP.Tools.Toonflow.Export)
  component(Exhub.MCP.Tools.Toonflow.MemoryAdd)
  component(Exhub.MCP.Tools.Toonflow.MemoryList)
  component(Exhub.MCP.Tools.Toonflow.MemorySearch)
  component(Exhub.MCP.Tools.Toonflow.MemoryIndex)
  component(Exhub.MCP.Tools.Toonflow.PipelineRun)
  component(Exhub.MCP.Tools.Toonflow.Chat)
  component(Exhub.MCP.Tools.Toonflow.ListAgents)

  @impl true
  def init(client_info, frame) do
    _ = client_info
    {:ok, frame}
  end

  @impl true
  def handle_request(request, frame) do
    Exhub.MCP.ServerHelpers.handle_request_with_filtered_tools(__MODULE__, request, frame)
  end
end
