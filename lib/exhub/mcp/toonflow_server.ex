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
    * `toonflow_list_scenes` / `toonflow_list_props` / `toonflow_generate_reference`
      — scene/prop library & reference images
    * `toonflow_generate_variation` / `toonflow_list_variations` /
      `toonflow_set_shot_variations` — wardrobe variations & per-shot costume
    * `toonflow_prompt_list` / `_get` / `_set` / `_history` / `_revert` —
      per-project prompt overrides (versioned)
    * `toonflow_worldview_set` / `toonflow_worldview_get` — always-injected
      worldview anchors
    * `toonflow_generate_storyboard` / `toonflow_list_shots` — shot planning
    * `toonflow_split_shot` — refine one shot into finer sub-shots
    * `toonflow_generate_image` — frame generation
    * `toonflow_generate_keyframe` / `toonflow_copy_keyframe` — start/end
      keyframes & cross-shot frame continuity
    * `toonflow_generate_shot_candidates` / `toonflow_select_candidate` —
      9-grid candidate compositions & ffmpeg cell crop
    * `toonflow_generate_video` / `toonflow_generate_voice` — clip & voice generation
    * `toonflow_assemble` / `toonflow_export` — FFmpeg episode assembly & packaging
    * `toonflow_pipeline_run` — orchestrate the whole pipeline (staged run/resume)
    * `toonflow_pipeline_plan` — read-only pre-review (stage status + per-shot
      readiness)
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
  component(Exhub.MCP.Tools.Toonflow.ListScenes)
  component(Exhub.MCP.Tools.Toonflow.ListProps)
  component(Exhub.MCP.Tools.Toonflow.GenerateReference)
  component(Exhub.MCP.Tools.Toonflow.GenerateVariation)
  component(Exhub.MCP.Tools.Toonflow.ListVariations)
  component(Exhub.MCP.Tools.Toonflow.SetShotVariations)
  component(Exhub.MCP.Tools.Toonflow.PromptList)
  component(Exhub.MCP.Tools.Toonflow.PromptGet)
  component(Exhub.MCP.Tools.Toonflow.PromptSet)
  component(Exhub.MCP.Tools.Toonflow.PromptHistory)
  component(Exhub.MCP.Tools.Toonflow.PromptRevert)
  component(Exhub.MCP.Tools.Toonflow.WorldviewSet)
  component(Exhub.MCP.Tools.Toonflow.WorldviewGet)
  component(Exhub.MCP.Tools.Toonflow.GenerateStoryboard)
  component(Exhub.MCP.Tools.Toonflow.ListShots)
  component(Exhub.MCP.Tools.Toonflow.SplitShot)
  component(Exhub.MCP.Tools.Toonflow.GenerateImage)
  component(Exhub.MCP.Tools.Toonflow.GenerateKeyframe)
  component(Exhub.MCP.Tools.Toonflow.CopyKeyframe)
  component(Exhub.MCP.Tools.Toonflow.GenerateShotCandidates)
  component(Exhub.MCP.Tools.Toonflow.SelectCandidate)
  component(Exhub.MCP.Tools.Toonflow.GenerateVideo)
  component(Exhub.MCP.Tools.Toonflow.GenerateVoice)
  component(Exhub.MCP.Tools.Toonflow.Assemble)
  component(Exhub.MCP.Tools.Toonflow.Export)
  component(Exhub.MCP.Tools.Toonflow.MemoryAdd)
  component(Exhub.MCP.Tools.Toonflow.MemoryList)
  component(Exhub.MCP.Tools.Toonflow.MemorySearch)
  component(Exhub.MCP.Tools.Toonflow.MemoryIndex)
  component(Exhub.MCP.Tools.Toonflow.PipelineRun)
  component(Exhub.MCP.Tools.Toonflow.PipelinePlan)
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
