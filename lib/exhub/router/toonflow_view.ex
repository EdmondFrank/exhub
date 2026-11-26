defmodule Exhub.Router.ToonflowView do
  @moduledoc """
  HTML views for the Toonflow canvas (Phase 5).

  Server-rendered shells (same convention as `DashboardView` / `AgentHubView`):
  inline CSS/JS in a heredoc, dark theme, all data fetched by the page from the
  `/toonflow/api/...` endpoints and the `/toonflow/ws` websocket.

    * `render_index/1` — project list + create form;
    * `render_project/1` — the canvas: stage rail, shot grid, inspector,
      live log and run controls.

  Both builders are pure (the caller passes the project list), so they unit-test
  without the `Store` running.
  """

  @doc "Render the project index page from a list of project summaries."
  @spec render_index([map()]) :: String.t()
  def render_index(projects \\ []) when is_list(projects) do
    cards =
      case projects do
        [] ->
          ~s(<p class="empty">No projects yet. Create one to get started.</p>)

        projects ->
          Enum.map_join(projects, "\n", &card/1)
      end

    """
    <!DOCTYPE html>
    <html lang="en">
    <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>Toonflow — Projects</title>
      #{head_style()}
    </head>
    <body>
      <header>
        <div class="wrap">
          <div>
            <h1>Toonflow</h1>
            <div class="subtitle">AI short-drama pipeline — novel → script → storyboard → video</div>
          </div>
          <div class="header-right"><a class="nav-link" href="/dashboard">Dashboard</a></div>
        </div>
      </header>
      <div class="wrap">
        <section class="create">
          <h2>New project</h2>
          <form id="create-form" class="row">
            <input id="create-name" type="text" placeholder="project-slug (a-z0-9._-)" required>
            <input id="create-desc" type="text" placeholder="description (optional)">
            <button type="submit">Create</button>
          </form>
          <div id="create-msg" class="msg"></div>
        </section>
        <section class="projects">
          <h2>Projects</h2>
          <div class="cards">
            #{cards}
          </div>
        </section>
      </div>
      <script>
        #{index_script()}
      </script>
    </body>
    </html>
    """
  end

  @doc "Render the canvas page for one project (data loaded client-side)."
  @spec render_project(String.t()) :: String.t()
  def render_project(name) when is_binary(name) do
    escaped = h(name)

    """
    <!DOCTYPE html>
    <html lang="en">
    <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>Toonflow — #{escaped}</title>
      #{head_style()}
      <style>
        .layout { display: grid; grid-template-columns: 220px minmax(0, 1fr) 340px; gap: 16px; }
        @media (max-width: 1100px) { .layout { grid-template-columns: 1fr; } }
        .panel { background: #161b22; border: 1px solid #30363d; border-radius: 10px; padding: 14px; }
        .panel h2 { font-size: 13px; text-transform: uppercase; letter-spacing: .04em; color: #8b949e; margin-bottom: 10px; }
        .rail ol { list-style: none; }
        .rail li { display: flex; align-items: center; justify-content: space-between; gap: 8px; padding: 6px 0; border-bottom: 1px solid #21262d; font-size: 13px; }
        .controls .row { display: flex; flex-wrap: wrap; gap: 8px; align-items: center; margin-bottom: 8px; }
        .controls input[type=text] { min-width: 260px; flex: 1; }
        .stage-pick { display: flex; flex-wrap: wrap; gap: 10px; font-size: 12px; color: #8b949e; }
        .stage-pick label { display: inline-flex; align-items: center; gap: 4px; }
        .grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(150px, 1fr)); gap: 10px; }
        .shot { background: #0d1117; border: 1px solid #30363d; border-radius: 8px; overflow: hidden; cursor: pointer; transition: border-color .15s, transform .15s; position: relative; }
        .shot:hover { border-color: #58a6ff; transform: translateY(-2px); }
        .shot.selected { border-color: #58a6ff; box-shadow: 0 0 0 1px #58a6ff; }
        .thumb { width: 100%; aspect-ratio: 16 / 9; object-fit: cover; background: #21262d; display: block; }
        .thumb.placeholder { display: flex; align-items: center; justify-content: center; color: #6e7681; font-size: 12px; }
        .shot-meta { padding: 8px; }
        .shot-meta .idx { font-size: 11px; color: #8b949e; }
        .shot-meta .desc { font-size: 12px; margin-top: 2px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
        .flags { display: flex; gap: 4px; margin-top: 6px; }
        .flag { font-size: 10px; padding: 1px 5px; border-radius: 999px; background: #21262d; color: #6e7681; }
        .flag.on { background: #1f6feb33; color: #79c0ff; }
        .log { font-family: ui-monospace, SFMono-Regular, Menlo, monospace; font-size: 11px; line-height: 1.5; height: 260px; overflow-y: auto; background: #0d1117; border-radius: 8px; padding: 8px; }
        .log div { white-space: pre-wrap; word-break: break-word; }
        .log .t { color: #6e7681; }
        .log .s-ok { color: #3fb950; }
        .log .s-error { color: #f85149; }
        .log .s-started { color: #79c0ff; }
        .log .s-skipped { color: #d29922; }
        .inspector .field { margin-bottom: 8px; }
        .inspector label { display: block; font-size: 11px; color: #8b949e; margin-bottom: 3px; }
        .inspector input[type=text], .inspector select, .inspector textarea { width: 100%; background: #0d1117; border: 1px solid #30363d; border-radius: 6px; color: #c9d1d9; padding: 5px 8px; font-size: 12px; font-family: inherit; }
        .inspector textarea { resize: vertical; min-height: 52px; line-height: 1.45; }
        .inspector input[type=text]:focus, .inspector select:focus, .inspector textarea:focus { outline: none; border-color: #58a6ff; }
        .inspector .pair { display: grid; grid-template-columns: 1fr 1fr; gap: 8px; }
        .inspector .btns { display: flex; flex-wrap: wrap; gap: 6px; align-items: center; margin-top: 10px; }
        .inspector .btns button, .inspector .render-row button { padding: 4px 10px; font-size: 12px; }
        .inspector .btns .dirty { font-size: 11px; color: #d29922; margin-left: auto; }
        .inspector .render-row { display: flex; flex-wrap: wrap; gap: 6px; align-items: center; margin-top: 10px; }
        .inspector .render-row .label { font-size: 11px; color: #8b949e; }
        .inspector .render-row button { background: transparent; border-color: #30363d; color: #c9d1d9; padding: 3px 8px; }
        .inspector .render-row button:hover { border-color: #58a6ff; color: #58a6ff; }
        .inspector .stale { display: flex; flex-wrap: wrap; gap: 4px; margin-top: 8px; }
        .inspector .stale span { font-size: 10px; padding: 1px 6px; border-radius: 999px; background: #d2992222; color: #d29922; border: 1px solid #d2992244; }
        .inspector .ai-hint { font-size: 10px; color: #a371f7; margin-bottom: 6px; }
        .inspector .ai-row { display: flex; flex-wrap: wrap; gap: 4px; align-items: center; margin-top: 4px; }
        .inspector .ai-row .ai-label { font-size: 10px; color: #a371f7; letter-spacing: .04em; }
        .inspector .ai-row button { background: transparent; border: 1px solid #30363d; color: #8b949e; padding: 1px 7px; font-size: 10px; border-radius: 999px; }
        .inspector .ai-row button:hover:not(:disabled) { border-color: #a371f7; color: #a371f7; }
        .inspector .ai-row button:disabled { opacity: .45; cursor: default; }
        .inspector .ai-row .ai-msg { font-size: 10px; color: #6e7681; margin-left: auto; }
        .inspector .ai-row .ai-msg.busy { color: #a371f7; }
        .inspector .ai-row .ai-msg.error { color: #f85149; }
        .inspector summary { font-size: 11px; color: #8b949e; cursor: pointer; margin: 10px 0 6px; }
        .inspector .msg { font-size: 11px; min-height: 14px; }
        .inspector img, .inspector video { width: 100%; border-radius: 8px; margin-top: 8px; background: #0d1117; }
        .flag.stale { background: #d2992233; color: #d29922; }
        .flag.live { background: #1f6feb; color: #fff; font-variant-numeric: tabular-nums; }
        .shot .play { position: absolute; left: 8px; top: 8px; font-size: 11px; color: #fff; background: #000a; border-radius: 999px; padding: 1px 7px; }
        .outputs li { display: flex; justify-content: space-between; gap: 8px; font-size: 13px; padding: 4px 0; border-bottom: 1px solid #21262d; }
        .conn { font-size: 11px; color: #8b949e; }
        .conn.on { color: #3fb950; }
        .tabs { display: flex; gap: 6px; margin: 0 0 12px; flex-wrap: wrap; }
        .tabs button { background: #161b22; border: 1px solid #30363d; color: #8b949e; padding: 6px 14px; border-radius: 999px; cursor: pointer; font-size: 13px; }
        .tabs button.active { background: #1f6feb; border-color: #1f6feb; color: #fff; }
        .tab-pane { display: none; }
        .tab-pane.active { display: block; }
        .subject-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(240px, 1fr)); gap: 12px; }
        .subject { background: #0d1117; border: 1px solid #30363d; border-radius: 10px; overflow: hidden; }
        .subject img { width: 100%; aspect-ratio: 1 / 1; object-fit: cover; background: #161b22; display: block; cursor: zoom-in; }
        .subject .body { padding: 10px; }
        .subject .name { font-weight: 600; color: #e6edf3; font-size: 14px; }
        .subject .text { color: #8b949e; font-size: 12px; line-height: 1.45; margin-top: 4px; max-height: 9em; overflow: auto; }
        .subject textarea { width: 100%; min-height: 96px; margin-top: 6px; padding: 6px 8px; background: #161b22; border: 1px solid #30363d; border-radius: 6px; color: #c9d1d9; font: inherit; font-size: 12px; line-height: 1.45; resize: vertical; }
        .subject textarea:focus { outline: none; border-color: #58a6ff; }
        .subject .row { display: flex; flex-wrap: wrap; gap: 6px; margin-top: 8px; }
        .subject .row button { padding: 4px 10px; font-size: 12px; }
        .subject .row button:disabled { opacity: .55; cursor: progress; }
        .subject .status { min-height: 15px; margin-top: 6px; font-size: 11px; color: #8b949e; }
        .subject .status.ok { color: #3fb950; }
        .subject .status.error { color: #f85149; }
        .subject .status.busy { color: #d29922; }
        .subject .noref { aspect-ratio: 1 / 1; display: flex; align-items: center; justify-content: center; color: #6e7681; font-size: 12px; background: #161b22; }
        .subject .row button.danger { color: #f85149; }
        .asset-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(200px, 1fr)); gap: 10px; }
        .asset { position: relative; background: #0d1117; border: 1px solid #30363d; border-radius: 8px; overflow: hidden; }
        .asset-del { position: absolute; top: 6px; right: 6px; width: 24px; height: 24px; padding: 0; border-radius: 999px; border: 1px solid #30363d; background: #0d1117cc; color: #c9d1d9; font-size: 13px; line-height: 1; cursor: pointer; opacity: .45; }
        .asset:hover .asset-del { opacity: 1; }
        .asset-del:hover { border-color: #f85149; color: #f85149; }
        .asset img, .asset video { width: 100%; aspect-ratio: 16 / 9; object-fit: cover; display: block; background: #161b22; }
        .asset img { cursor: zoom-in; }
        .asset .meta { padding: 6px 8px; font-size: 11px; color: #8b949e; display: flex; justify-content: space-between; gap: 6px; }
        .asset .meta .kind { color: #79c0ff; }
        .lightbox { position: fixed; inset: 0; background: #000c; display: none; align-items: center; justify-content: center; z-index: 100; padding: 24px; }
        .lightbox.on { display: flex; }
        .lightbox img, .lightbox video { max-width: 92vw; max-height: 88vh; border-radius: 10px; }
        .lightbox .close { position: absolute; top: 14px; right: 22px; font-size: 30px; color: #fff; cursor: pointer; line-height: 1; }
      </style>
    </head>
    <body data-project="#{escaped}">
      <header>
        <div class="wrap">
          <div>
            <h1><a class="nav-link" href="/toonflow">Toonflow</a> / #{escaped}</h1>
            <div class="subtitle" id="subtitle">loading…</div>
          </div>
          <div class="header-right">
            <span class="conn" id="conn">disconnected</span>
            <button id="refresh" class="ghost">Refresh</button>
          </div>
        </div>
      </header>
      <div class="wrap">
        <div class="tabs">
          <button class="tab active" data-tab="storyboard">分镜</button>
          <button class="tab" data-tab="cast">角色 &amp; 场景</button>
          <button class="tab" data-tab="assets">物料</button>
        </div>
        <section class="tab-pane active" id="tab-storyboard">
        <div class="layout">
          <aside class="panel rail">
            <h2>Stages</h2>
            <ol id="rail"></ol>
          </aside>
          <main class="panel canvas">
            <section class="controls">
              <div class="row">
                <input id="novel-path" type="text" placeholder="novel file path (optional; needed for a first run)">
                <label class="stage-pick"><input type="checkbox" id="opt-resume" checked> resume</label>
                <label class="stage-pick"><input type="checkbox" id="opt-continue"> continue on error</label>
                <button id="run">Run pipeline</button>
              </div>
              <div class="stage-pick" id="stage-pick"></div>
              <div id="run-msg" class="msg"></div>
            </section>
            <h2>Shots</h2>
            <div id="shots" class="grid"></div>
          </main>
          <aside class="panel">
            <h2>Inspector</h2>
            <div id="inspector" class="inspector"><p class="empty">Select a shot.</p></div>
            <h2 style="margin-top:16px">Live progress</h2>
            <div id="log" class="log"></div>
            <h2 style="margin-top:16px">Outputs</h2>
            <ul id="outputs" class="outputs"></ul>
          </aside>
        </div>
        </section>
        <section class="tab-pane" id="tab-cast">
          <div class="panel"><h2>角色</h2><div id="cast-characters" class="subject-grid"></div></div>
          <div class="panel" style="margin-top:12px"><h2>场景</h2><div id="cast-scenes" class="subject-grid"></div></div>
          <div class="panel" style="margin-top:12px"><h2>道具</h2><div id="cast-props" class="subject-grid"></div></div>
        </section>
        <section class="tab-pane" id="tab-assets">
          <div class="panel"><h2>物料</h2><div id="asset-grid" class="asset-grid"></div></div>
        </section>
      </div>
      <div id="lightbox" class="lightbox"><span class="close" onclick="document.getElementById('lightbox').classList.remove('on')">×</span><div id="lightbox-body"></div></div>
      <script>
        #{project_script()}
      </script>
    </body>
    </html>
    """
  end

  # ── partials ─────────────────────────────────────────────────────────

  defp card(project) do
    counts = project["counts"] || %{}
    meta = project["meta"] || %{}

    """
    <a class="card" href="/toonflow/projects/#{h(project["name"])}">
      <div class="card-title">#{h(project["name"])}</div>
      <div class="card-desc">#{h(meta["description"] || "")}</div>
      <div class="card-counts">
        <span>#{counts["chapters"] || 0} chapters</span>
        <span>#{counts["images"] || 0} images</span>
        <span>#{counts["videos"] || 0} clips</span>
        <span>#{counts["output"] || 0} outputs</span>
      </div>
    </a>
    """
  end

  defp head_style do
    """
    <style>
      * { box-sizing: border-box; margin: 0; padding: 0; }
      body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; background: #0d1117; color: #c9d1d9; line-height: 1.6; }
      .wrap { max-width: 1400px; margin: 0 auto; padding: 16px; }
      header { background: #161b22; border-bottom: 1px solid #30363d; margin-bottom: 20px; }
      header .wrap { display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 8px; }
      h1 { color: #58a6ff; font-size: 22px; }
      h1 a { color: inherit; }
      h2 { color: #c9d1d9; font-size: 15px; }
      .subtitle { color: #8b949e; font-size: 13px; }
      .header-right { display: flex; align-items: center; gap: 10px; }
      .nav-link { color: #58a6ff; text-decoration: none; font-size: 13px; }
      .nav-link:hover { text-decoration: underline; }
      input[type=text] { background: #0d1117; border: 1px solid #30363d; border-radius: 6px; color: #c9d1d9; padding: 6px 10px; font-size: 13px; }
      input[type=text]:focus { outline: none; border-color: #58a6ff; }
      button { background: #238636; border: 1px solid #2ea043; border-radius: 6px; color: #fff; padding: 6px 14px; font-size: 13px; cursor: pointer; }
      button:hover { background: #2ea043; }
      button.ghost { background: transparent; border-color: #30363d; color: #c9d1d9; }
      button.ghost:hover { border-color: #58a6ff; color: #58a6ff; }
      .msg { font-size: 13px; margin-top: 8px; min-height: 18px; }
      .msg.ok { color: #3fb950; }
      .msg.error { color: #f85149; }
      .empty { color: #8b949e; font-size: 13px; }
      .badge { font-size: 11px; padding: 1px 8px; border-radius: 999px; border: 1px solid transparent; }
      .badge.done { background: #23863633; color: #3fb950; border-color: #2ea04355; }
      .badge.partial { background: #d2992233; color: #d29922; border-color: #d2992255; }
      .badge.ready { background: #1f6feb33; color: #79c0ff; border-color: #1f6feb55; }
      .badge.blocked { background: #21262d; color: #6e7681; }
      .badge.error { background: #f8514933; color: #f85149; border-color: #f8514955; }
      .cards { display: grid; grid-template-columns: repeat(auto-fill, minmax(240px, 1fr)); gap: 12px; }
      .card { display: block; background: #161b22; border: 1px solid #30363d; border-radius: 10px; padding: 14px; text-decoration: none; color: inherit; transition: transform .15s, border-color .15s; }
      .card:hover { transform: translateY(-2px); border-color: #58a6ff; }
      .card-title { color: #58a6ff; font-weight: 600; }
      .card-desc { color: #8b949e; font-size: 12px; min-height: 18px; }
      .card-counts { display: flex; flex-wrap: wrap; gap: 8px; margin-top: 8px; color: #8b949e; font-size: 11px; }
      .create { margin-bottom: 24px; }
      .create .row { display: flex; gap: 8px; margin-top: 8px; flex-wrap: wrap; }
      .create input { min-width: 200px; }
    </style>
    """
  end

  defp index_script do
    """
    const form = document.getElementById('create-form');
    const msg = document.getElementById('create-msg');
    form.addEventListener('submit', async (e) => {
      e.preventDefault();
      const name = document.getElementById('create-name').value.trim();
      const description = document.getElementById('create-desc').value.trim();
      msg.className = 'msg'; msg.textContent = 'creating…';
      try {
        const r = await fetch('/toonflow/api/projects', {
          method: 'POST',
          headers: { 'content-type': 'application/json' },
          body: JSON.stringify({ name, description })
        });
        const data = await r.json();
        if (!r.ok || data.error) throw new Error(data.error || ('HTTP ' + r.status));
        msg.className = 'msg ok'; msg.textContent = 'created — opening…';
        location.href = '/toonflow/projects/' + encodeURIComponent((data.project || {}).name || name);
      } catch (err) {
        msg.className = 'msg error'; msg.textContent = String(err.message || err);
      }
    });
    """
  end

  defp project_script do
    """
    const project = document.body.dataset.project;
    const STAGES = #{Jason.encode!(Exhub.Toonflow.Pipeline.stages())};
    const SHOT_SIZES = #{Jason.encode!(Exhub.Toonflow.Storyboard.size_options())};
    const SHOT_MOVES = #{Jason.encode!(Exhub.Toonflow.Storyboard.motion_options())};
    const CAMERA_HINTS = #{Jason.encode!(Exhub.Toonflow.Storyboard.camera_options())};
    // The inspector's AI buttons: the actions, the fields that accept them, and
    // the configured director model they run on (see ShotAssist, toonflow.agents).
    const DIRECTOR_MODEL = #{Jason.encode!(Exhub.Toonflow.Config.agent_model("director") || "default")};
    const AI_FIELDS = #{Jason.encode!(Exhub.Toonflow.ShotAssist.fields())};
    const AI_ACTIONS = #{Jason.encode!(Enum.map(Exhub.Toonflow.ShotAssist.actions(), &[&1, Exhub.Toonflow.ShotAssist.action_label(&1)]))};
    const state = { snap: null, shots: [], selected: null, cast: [], dirty: false, inspFor: null, ws: null, stages: null, retries: 0, live: {}, subj: {} };

    // Tabs: 分镜 (existing canvas) / 角色 & 场景 / 物料.
    document.querySelectorAll('.tab').forEach((b) => b.addEventListener('click', () => {
      document.querySelectorAll('.tab').forEach((x) => x.classList.toggle('active', x === b));
      document.querySelectorAll('.tab-pane').forEach((p) => p.classList.toggle('active', p.id === 'tab-' + b.dataset.tab));
    }));

    const el = (id) => document.getElementById(id);
    const esc = (s) => String(s == null ? '' : s).replace(/[&<>"']/g, (c) => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
    const fmtSize = (n) => {
      if (!n) return '';
      const u = ['B','KB','MB','GB']; let i = 0; let v = n;
      while (v >= 1024 && i < u.length - 1) { v /= 1024; i++; }
      return v.toFixed(i ? 1 : 0) + u[i];
    };

    function log(text, status) {
      const box = el('log');
      const line = document.createElement('div');
      const t = document.createElement('span');
      t.className = 't'; t.textContent = new Date().toLocaleTimeString() + ' ';
      const s = document.createElement('span');
      s.className = status ? ('s-' + status) : '';
      s.textContent = text;
      line.appendChild(t); line.appendChild(s);
      box.appendChild(line);
      box.scrollTop = box.scrollHeight;
      while (box.childNodes.length > 400) box.removeChild(box.firstChild);
    }

    // Renders have no percentage to show: MoArk reports only queued → running →
    // success, and a clip takes minutes. So the canvas keeps its own per-shot
    // clock — a `started`/`running` event sets the mark and a 1s ticker walks it
    // forward, which keeps the badge alive even between 10s poll ticks.
    const fmtClock = (secs) => {
      const s = Math.max(0, Math.floor(secs));
      return Math.floor(s / 60) + ':' + String(s % 60).padStart(2, '0');
    };

    function markLive(id, stage, status) {
      const prev = state.live[id];
      const same = prev && prev.stage === stage;
      state.live[id] = { stage, status: status || 'running', at: same ? prev.at : Date.now() };
      return prev;
    }

    function clearLive(id) { delete state.live[id]; }

    function liveBadge(shot) {
      const l = state.live[shot.id];
      if (!l) return '';
      const secs = (Date.now() - l.at) / 1000;
      const title = `${l.stage} ${l.status} · ${fmtClock(secs)}`;
      return `<span class="flag live" title="${esc(title)}">▶ ${esc(l.stage)} ${fmtClock(secs)}</span>`;
    }

    // Repaint only the flags cell of the shots that are rendering, so the badge
    // ticks without rebuilding the grid (or the inspector form being typed in).
    function paintLive() {
      Object.keys(state.live).forEach((id) => {
        const node = document.querySelector('.shot[data-id="' + id + '"] .flags');
        const shot = state.shots.find((s) => s.id === id);
        if (node && shot) node.innerHTML = flags(shot);
      });
    }
    setInterval(paintLive, 1000);

    // A snapshot is the only thing that carries new asset URLs, so a finished
    // render asks for one instead of waiting for the next reconnect.
    let refreshTimer = null;
    function requestRefresh() {
      if (refreshTimer) return;
      refreshTimer = setTimeout(() => {
        refreshTimer = null;
        if (state.ws && state.ws.readyState === 1) state.ws.send(JSON.stringify({ action: 'refresh' }));
      }, 800);
    }

    async function api(path, opts) {
      const r = await fetch(path, opts);
      const text = await r.text();
      let data = null;
      try { data = text ? JSON.parse(text) : null; } catch (_) { data = { raw: text }; }
      if (!r.ok) throw new Error((data && data.error) || ('HTTP ' + r.status));
      return data;
    }

    function renderSubtitle() {
      const s = state.snap; if (!s) return;
      const c = s.counts || {};
      el('subtitle').textContent = (s.exists ? '' : 'missing directory — ') +
        `${c.chapters||0} chapters · ${c.images||0} images · ${c.videos||0} clips · ${c.audio||0} audio · ${c.output||0} outputs`;
    }

    function renderRail() {
      const plan = (state.snap && state.snap.plan) || { stages: [] };
      const byStage = {};
      (plan.stages || []).forEach((s) => { byStage[s.stage] = s.status; });
      el('rail').innerHTML = STAGES.map((stage) => {
        const st = byStage[stage] || 'blocked';
        return `<li><span>${esc(stage)}</span><span class="badge ${esc(st)}">${esc(st)}</span></li>`;
      }).join('');
      const next = plan.next ? `next: ${plan.next}` : 'all stages done';
      el('subtitle').title = next;
    }

    function renderStagePick() {
      const pick = el('stage-pick');
      if (pick.dataset.ready) return;
      pick.dataset.ready = '1';
      pick.innerHTML = STAGES.map((s) =>
        `<label><input type="checkbox" class="stage-box" value="${s}" checked> ${esc(s)}</label>`
      ).join('');
    }

    function thumb(shot) {
      // The still stands in for the clip (a <video> per card would hammer the
      // disk), but a play hint says the clip is there and is previewable.
      const play = shot.video ? '<span class="play">▶ clip</span>' : '';
      if (shot.image) return `<img class="thumb" loading="lazy" src="${esc(shot.image)}" alt="">` + play;
      if (shot.video) return `<video class="thumb" muted preload="metadata" src="${esc(shot.video)}"></video>` + play;
      return `<div class="thumb placeholder">no frame</div>`;
    }

    function flags(shot) {
      const stale = (shot.stale || []).length ? '<span class="flag stale">stale</span>' : '';
      return `<span class="flag ${shot.image ? 'on' : ''}">img</span>` +
             `<span class="flag ${shot.video ? 'on' : ''}">vid</span>` +
             `<span class="flag ${shot.audio ? 'on' : ''}">aud</span>` + stale + liveBadge(shot);
    }

    function renderShots() {
      const shots = state.shots;
      const box = el('shots');
      if (!shots.length) { box.innerHTML = '<p class="empty">No shots yet — run the storyboard stage.</p>'; return; }
      box.innerHTML = shots.map((shot) => `
        <div class="shot ${state.selected === shot.id ? 'selected' : ''}" data-id="${esc(shot.id)}">
          ${thumb(shot)}
          <div class="shot-meta">
            <div class="idx">#${shot.idx} · ${esc(shot.size || '')}</div>
            <div class="desc" title="${esc(shot.description || '')}">${esc(shot.description || '')}</div>
            <div class="flags">${flags(shot)}</div>
          </div>
        </div>`).join('');
      box.querySelectorAll('.shot').forEach((node) => {
        node.addEventListener('click', () => {
          if (state.dirty && state.selected !== node.dataset.id && !confirm('Discard unsaved edits?')) return;
          state.selected = node.dataset.id; state.dirty = false; renderShots(); renderInspector();
        });
      });
    }

    // The inspector edits a shot in place; `description`/`keyframe_*_prompt` are the
    // canvas' names for `shot_desc`/`meta.keyframe_*_prompt` (see Storyboard.update_shot/4).
    const SHOT_FIELDS = ['scene', 'description', 'size', 'motion', 'camera', 'lighting', 'characters', 'speaker', 'dialogue', 'prompt'];
    const SHOT_ADVANCED = ['keyframe_start_prompt', 'keyframe_end_prompt'];

    function shotValue(shot, name) {
      const kf = shot.keyframe || {};
      if (name === 'characters') return (shot.characters || []).join('、');
      if (name === 'keyframe_start_prompt') return kf.start_prompt || '';
      if (name === 'keyframe_end_prompt') return kf.end_prompt || '';
      return shot[name] == null ? '' : String(shot[name]);
    }

    function optionList(values, current, blank) {
      const all = values.slice();
      if (current && all.indexOf(current) === -1) all.push(current);
      const head = blank == null ? '' : `<option value="">${esc(blank)}</option>`;
      return head + all.map((v) => `<option value="${esc(v)}"${v === current ? ' selected' : ''}>${esc(v)}</option>`).join('');
    }

    // Free-text fields with autocomplete: the list is a hint, anything typed is kept.
    // `characters` reads the cast lazily — a new snapshot replaces state.cast.
    const PLACEHOLDERS = { camera: '如 俯拍 / 过肩 / 低角度仰拍', characters: '名字，用、或,分隔' };
    const hintsFor = (name) => (name === 'camera' ? CAMERA_HINTS : name === 'characters' ? state.cast : []);

    function fieldInput(shot, name, label, rows) {
      const value = shotValue(shot, name);
      const hints = hintsFor(name) || [];
      const list = hints.length ? ` list="${name}-options"` : '';
      const ph = PLACEHOLDERS[name] ? ` placeholder="${PLACEHOLDERS[name]}"` : '';
      let control;
      if (name === 'size') control = `<select id="f-${name}">${optionList(SHOT_SIZES, value, '—')}</select>`;
      else if (name === 'motion') control = `<select id="f-${name}">${optionList(SHOT_MOVES, value, '—')}</select>`;
      else if (name === 'speaker') control = `<select id="f-${name}">${optionList(state.cast, value, '— none')}</select>`;
      else if (rows) control = `<textarea id="f-${name}" rows="${rows}"${ph}>${esc(value)}</textarea>`;
      else control = `<input id="f-${name}" type="text"${list}${ph} value="${esc(value)}">`;
      const datalist = hints.length
        ? `<datalist id="${name}-options">${hints.map((n) => `<option value="${esc(n)}">`).join('')}</datalist>`
        : '';
      return `<div class="field"><label for="f-${name}">${esc(label)}</label>${control}${datalist}${aiRow(name)}</div>`;
    }

    // The AI toolbar under one prose field. `aiAssist` writes its answer back
    // into the textarea/input and marks the form dirty; Save is still explicit.
    function aiRow(name) {
      if (!AI_FIELDS.includes(name)) return '';
      const buttons = AI_ACTIONS.map(([action, label]) =>
        `<button type="button" data-action="${esc(action)}" title="导演模型：${esc(DIRECTOR_MODEL)}">${esc(label)}</button>`
      ).join('');
      return `<div class="ai-row" data-field="${esc(name)}"><span class="ai-label">AI</span>${buttons}<span class="ai-msg"></span></div>`;
    }

    function inspectorForm(shot) {
      return `
        <form id="insp-form" autocomplete="off">
          <div class="ai-hint">AI 助手 · ${esc(DIRECTOR_MODEL)}</div>
          <div class="field"><label>Shot #${shot.idx} · ${esc(shot.scene || '')}</label></div>
          ${fieldInput(shot, 'scene', 'Scene')}
          ${fieldInput(shot, 'description', 'Description', 2)}
          <div class="pair">${fieldInput(shot, 'size', 'Shot size')}${fieldInput(shot, 'motion', 'Move')}</div>
          <div class="pair">${fieldInput(shot, 'camera', 'Camera')}${fieldInput(shot, 'lighting', 'Lighting')}</div>
          ${fieldInput(shot, 'characters', 'Characters')}
          ${fieldInput(shot, 'speaker', 'Speaker')}
          ${fieldInput(shot, 'dialogue', 'Dialogue', 2)}
          ${fieldInput(shot, 'prompt', 'Prompt', 3)}
          <details><summary>Keyframe prompts</summary>
            ${fieldInput(shot, 'keyframe_start_prompt', 'Start frame', 3)}
            ${fieldInput(shot, 'keyframe_end_prompt', 'End frame (blank = none)', 3)}
          </details>
          <div class="btns">
            <button type="submit" id="insp-save" disabled>Save</button>
            <button type="button" class="ghost" id="insp-reset">Reset</button>
            <span class="dirty" id="insp-dirty"></span>
          </div>
          <div class="msg" id="insp-msg"></div>
          <div class="stale" id="insp-stale"></div>
          <div class="render-row">
            <span class="label">re-render</span>
            <button type="button" data-kind="keyframe">Keyframes</button>
            <button type="button" data-kind="image">Still</button>
            <button type="button" data-kind="video">Clip</button>
            <button type="button" data-kind="voice">Voice</button>
          </div>
        </form>
        <div id="insp-media"></div>`;
    }

    function shotPatch(shot) {
      const patch = {};
      SHOT_FIELDS.concat(SHOT_ADVANCED).forEach((name) => {
        const node = el('f-' + name);
        if (!node) return;
        const value = node.value.trim();
        if (value === shotValue(shot, name)) return;
        if (name === 'characters') {
          patch[name] = value ? value.split(/[,，、;；]/).map((s) => s.trim()).filter(Boolean) : [];
        } else {
          patch[name] = value === '' ? null : value;
        }
      });
      return patch;
    }

    function renderInspectorMedia() {
      const shot = state.shots.find((s) => s.id === state.selected);
      const media = el('insp-media');
      if (!shot || !media) return;

      // Every kind is shown, not just one: a shot normally has a still *and* a
      // clip, and the clip used to be unreachable once the still existed.
      // Re-rendering only on a media change keeps a playing clip from restarting
      // on the next snapshot.
      const sig = [shot.id, shot.image, shot.video, shot.audio].join('|');
      if (media.dataset.sig !== sig) {
        const parts = [];
        if (shot.image) parts.push(`<img src="${esc(shot.image)}" alt="">`);
        if (shot.video) parts.push(`<video controls preload="metadata" src="${esc(shot.video)}"></video>`);
        if (shot.audio) parts.push(`<audio controls style="width:100%;margin-top:6px" src="${esc(shot.audio)}"></audio>`);
        media.dataset.sig = sig;
        media.innerHTML = parts.length ? parts.join('') : '<p class="empty">No media yet.</p>';
      }

      const kinds = shot.stale || [];
      el('insp-stale').innerHTML = kinds.map((k) => `<span>${esc(k)} stale</span>`).join('');
    }

    function renderInspector(force) {
      const shot = state.shots.find((s) => s.id === state.selected);
      const box = el('inspector');
      if (!shot) {
        box.innerHTML = '<p class="empty">Select a shot.</p>';
        state.inspFor = null; state.dirty = false;
        return;
      }
      // A snapshot arrives after every shot event; rebuilding the form would
      // throw away what is being typed, so only the media follows project state.
      if (!force && state.inspFor === shot.id && box.querySelector('#insp-form')) {
        renderInspectorMedia();
        return;
      }
      state.inspFor = shot.id;
      state.dirty = false;
      box.innerHTML = inspectorForm(shot);
      wireInspector(shot);
      renderInspectorMedia();
    }

    function wireInspector(shot) {
      const form = el('insp-form');

      const update = () => {
        const n = Object.keys(shotPatch(shot)).length;
        state.dirty = n > 0;
        el('insp-save').disabled = n === 0;
        el('insp-dirty').textContent = n ? n + ' unsaved' : '';
      };

      form.addEventListener('input', update);
      form.addEventListener('submit', (ev) => { ev.preventDefault(); saveShot(shot); });
      el('insp-reset').addEventListener('click', () => {
        state.dirty = false; state.inspFor = null; renderInspector(true);
      });
      form.querySelector('.render-row').addEventListener('click', (ev) => {
        const kind = ev.target && ev.target.dataset ? ev.target.dataset.kind : null;
        if (kind) renderShot(shot, kind);
      });

      form.addEventListener('click', (ev) => {
        const btn = ev.target && ev.target.closest ? ev.target.closest('.ai-row button[data-action]') : null;
        if (!btn) return;
        const row = btn.closest('.ai-row');
        aiAssist(shot, row.dataset.field, btn.dataset.action, row);
      });

      update();
    }

    // One-click AI rewrite of the field being edited. The server returns the
    // new text; `continue` appends it, every other action replaces the value.
    // Nothing is saved here — the input event marks the form dirty as usual.
    async function aiAssist(shot, field, action, row) {
      const node = el('f-' + field);
      if (!node || !row) return;
      const msg = row.querySelector('.ai-msg');
      const original = node.value;
      const text = original.trim();
      if (!text) {
        msg.className = 'ai-msg error';
        msg.textContent = '先输入内容';
        return;
      }

      const buttons = Array.from(row.querySelectorAll('button'));
      buttons.forEach((b) => { b.disabled = true; });
      msg.className = 'ai-msg busy';
      msg.textContent = '导演模型处理中…';

      try {
        const data = await api(`/toonflow/api/projects/${encodeURIComponent(project)}/shots/${encodeURIComponent(shot.id)}/assist`, {
          method: 'POST', headers: { 'content-type': 'application/json' },
          body: JSON.stringify({ field, action, text })
        });
        const result = String((data && data.text) || '').trim();
        if (!result) throw new Error('模型返回了空文本');
        // The user may have switched shots or typed while the model was busy;
        // never overwrite a field that moved on underneath the request.
        if (state.selected !== shot.id || !document.body.contains(node)) return;
        if (node.value !== original) {
          msg.className = 'ai-msg error';
          msg.textContent = '内容已变化，未覆盖';
          return;
        }
        if (action === 'continue') {
          const sep = /[\\n\\r]$/.test(original) ? '' : '\\n';
          node.value = original + sep + result;
        } else {
          node.value = result;
        }
        node.dispatchEvent(new Event('input', { bubbles: true }));
        node.focus();
        msg.className = 'ai-msg';
        msg.textContent = '✓ ' + ((data && data.model) || DIRECTOR_MODEL);
        log(`AI ${action} · ${field}`, 'ok');
      } catch (err) {
        msg.className = 'ai-msg error';
        msg.textContent = String(err.message || err);
        log(`AI ${action} failed: ${err.message}`, 'error');
      } finally {
        buttons.forEach((b) => { b.disabled = false; });
      }
    }

    async function saveShot(shot) {
      const patch = shotPatch(shot);
      if (!Object.keys(patch).length) return;
      const msg = el('insp-msg');
      msg.className = 'msg'; msg.textContent = 'saving…';

      try {
        const data = await api(`/toonflow/api/projects/${encodeURIComponent(project)}/shots/${encodeURIComponent(shot.id)}`, {
          method: 'POST', headers: { 'content-type': 'application/json' }, body: JSON.stringify(patch)
        });
        const fresh = data && data.shot;
        if (fresh) {
          const i = state.shots.findIndex((s) => s.id === fresh.id);
          if (i >= 0) state.shots[i] = fresh;
        }
        state.dirty = false; state.inspFor = null;
        renderShots(); renderInspector(true);
        log(`shot #${shot.idx} edited: ${Object.keys(patch).join(', ')}`, 'ok');
        const now = el('insp-msg');
        if (now) { now.className = 'msg ok'; now.textContent = 'saved'; }
      } catch (err) {
        msg.className = 'msg error'; msg.textContent = String(err.message || err);
        log(`edit failed: ${err.message}`, 'error');
      }
    }

    async function renderShot(shot, kind) {
      log(`re-render ${kind} for shot #${shot.idx}…`, 'started');
      try {
        const data = await api(`/toonflow/api/projects/${encodeURIComponent(project)}/shots/${encodeURIComponent(shot.id)}/render`, {
          method: 'POST', headers: { 'content-type': 'application/json' }, body: JSON.stringify({ kinds: [kind] })
        });
        log(`queued ${(data.started || []).join(', ')} — watch the shot badge`, 'ok');
      } catch (err) {
        log(`re-render failed: ${err.message}`, 'error');
      }
    }

    // ── 角色 & 物料 tabs ─────────────────────────────────────────────
    // A subject card is editable: the reference sheet can be replaced by an
    // upload, and the description it was rendered from can be rewritten and
    // re-rendered. The status line is kept in `state.subj` so it survives the
    // snapshot-driven re-render that follows a save.
    function subjectKey(kind, name) { return kind + '|' + (name || ''); }

    function subjectCard(s, empty, kind) {
      const ref = (s.refs || [])[0];
      const st = state.subj[subjectKey(kind, s.name)] || {};
      // `busy` is stored with the status, so a snapshot that rebuilds the cards
      // mid-render cannot hand the buttons back to the user.
      const dis = st.cls === 'busy' ? ' disabled' : '';
      const noRef = ref ? '' : ' disabled';
      const media = ref
        ? `<img loading="lazy" data-full="${esc(ref)}" src="${esc(ref)}" alt="${esc(s.name || '')}">`
        : `<div class="noref">${esc(empty)}</div>`;
      return `<div class="subject" data-kind="${esc(kind)}" data-name="${esc(s.name || '')}">
        ${media}
        <div class="body">
          <div class="name">${esc(s.name || '')}</div>
          <textarea class="subject-text" spellcheck="false" placeholder="外观 / 场景描述（参考图与后续分镜都据此生成）">${esc(s.text || '')}</textarea>
          <div class="row">
            <button class="ghost" data-act="upload"${dis}>上传图片</button>
            <button class="ghost" data-act="reverse"${dis}${noRef} title="用视觉模型从当前参考图反推描述">反推提示词</button>
          </div>
          <div class="row">
            <button data-act="regen"${dis}>保存并重新生成</button>
            <button class="ghost" data-act="save"${dis}>仅保存</button>
            <button class="ghost danger" data-act="clear"${dis}${noRef} title="删除该对象的参考图与文件">清除参考图</button>
          </div>
          <input type="file" accept="image/png,image/jpeg,image/webp" style="display:none">
          <div class="status ${esc(st.cls || '')}">${esc(st.text || '')}</div>
        </div>
      </div>`;
    }

    function renderCast() {
      // A snapshot can arrive while a description is being typed; rebuilding the
      // cards would discard the edit.
      const active = document.activeElement;
      if (active && active.classList && active.classList.contains('subject-text')) return;
      const snap = state.snap || {};
      const chars = snap.characters || [];
      const scenes = snap.scenes || [];
      const props = snap.props || [];
      el('cast-characters').innerHTML = chars.length
        ? chars.map((s) => subjectCard(s, 'no reference yet', 'character')).join('')
        : '<p class="empty">No characters yet — run the assets stage.</p>';
      el('cast-scenes').innerHTML = scenes.length
        ? scenes.map((s) => subjectCard(s, 'no reference yet', 'scene')).join('')
        : '<p class="empty">No scenes yet.</p>';
      el('cast-props').innerHTML = props.length
        ? props.map((s) => subjectCard(s, 'no reference yet', 'prop')).join('')
        : '<p class="empty">No props yet.</p>';
    }

    function findSubjectCard(kind, name) {
      const nodes = document.querySelectorAll('#tab-cast .subject');
      for (const node of nodes) {
        if (node.dataset.kind === kind && node.dataset.name === name) return node;
      }
      return null;
    }

    function subjectStatus(card, text, cls) {
      const st = { text, cls: cls || '' };
      state.subj[subjectKey(card.dataset.kind, card.dataset.name)] = st;
      const node = card.querySelector('.status');
      if (node) { node.textContent = text; node.className = 'status ' + (cls || ''); }
      // Keep the buttons in step: a snapshot rebuilds the cards from
      // `state.subj`, so a busy action must stay disabled across the refresh.
      const busy = (cls || '') === 'busy';
      card.querySelectorAll('button').forEach((b) => { b.disabled = busy; });
    }

    function subjectBusy(card, busy) {
      card.querySelectorAll('button').forEach((b) => { b.disabled = busy; });
    }

    // Edits travel over the page's websocket: it is already open and
    // authenticated, and the socket also carries the "started"/"ok"/"error"
    // frames that a 20-40 second render needs.
    function subjectSend(card, payload, pending) {
      if (!state.ws || state.ws.readyState !== 1) {
        subjectStatus(card, 'socket 未连接，刷新页面后重试', 'error');
        return;
      }
      subjectBusy(card, true);
      subjectStatus(card, pending, 'busy');
      state.ws.send(JSON.stringify(payload));
    }

    function saveSubject(card, regenerate) {
      const text = card.querySelector('.subject-text').value;
      subjectSend(card, {
        action: 'subject_reference',
        kind: card.dataset.kind,
        name: card.dataset.name,
        text, regenerate
      }, regenerate ? '生成中…（约 20-40 秒）' : '保存中…');
    }

    function onSubjectEvent(msg) {
      const card = findSubjectCard(msg.kind, msg.name);
      if (!card) return;
      if (msg.status === 'started') {
        const pending = msg.action === 'reverse' ? '正在根据参考图反推提示词…'
          : msg.action === 'save' ? '保存中…' : '生成中…（约 20-40 秒）';
        subjectStatus(card, pending, 'busy');
        return;
      }
      if (msg.status === 'error') {
        subjectStatus(card, msg.error || '操作失败', 'error');
        return;
      }
      if (msg.action === 'reverse') {
        const ta = card.querySelector('.subject-text');
        if (ta && msg.text) ta.value = msg.text;
        subjectStatus(card, '已根据参考图反推提示词，可编辑后重新生成', 'ok');
        requestRefresh();
        return;
      }
      if (msg.action === 'upload' && msg.reverse) {
        // The upload answers immediately and a `reverse` frame follows a moment
        // later; keep the card busy meanwhile so nothing is rendered from the
        // old description.
        subjectStatus(card, '已替换参考图，正在反推提示词…', 'busy');
        requestRefresh();
        return;
      }
      const done = msg.action === 'upload' ? '已替换参考图'
        : msg.action === 'clear' ? '已清除参考图'
        : msg.action === 'save' ? '已保存' : '已重新生成';
      subjectStatus(card, done, 'ok');
      requestRefresh();
    }

    // One websocket frame is capped at ~1MB, so the picked file is normalized
    // first: draw it at ≤1408px and re-encode as JPEG. That is plenty for a
    // conditioning image and keeps phone/camera files from being rejected.
    function shrinkImage(file) {
      return new Promise((resolve, reject) => {
        const img = new Image();
        const url = URL.createObjectURL(file);
        img.onload = () => {
          URL.revokeObjectURL(url);
          try {
            const max = 1408;
            const scale = Math.min(1, max / Math.max(img.width, img.height));
            const w = Math.max(1, Math.round(img.width * scale));
            const h = Math.max(1, Math.round(img.height * scale));
            const canvas = document.createElement('canvas');
            canvas.width = w; canvas.height = h;
            const ctx = canvas.getContext('2d');
            ctx.fillStyle = '#fff'; ctx.fillRect(0, 0, w, h);
            ctx.drawImage(img, 0, 0, w, h);
            let data = canvas.toDataURL('image/jpeg', 0.9).split(',')[1];
            if (data.length > 700000) data = canvas.toDataURL('image/jpeg', 0.72).split(',')[1];
            if (data.length > 900000) throw new Error('图片过大，请换一张');
            resolve(data);
          } catch (err) { reject(err); }
        };
        img.onerror = () => { URL.revokeObjectURL(url); reject(new Error('无法读取该图片')); };
        img.src = url;
      });
    }

    function clearSubject(card) {
      if (!window.confirm('删除该角色的全部参考图？文件会一并删除，且不可撤销。')) return;
      subjectSend(card, {
        action: 'subject_clear',
        kind: card.dataset.kind,
        name: card.dataset.name
      }, '清除中…');
    }

    async function uploadSubject(card, file) {
      subjectBusy(card, true);
      subjectStatus(card, '处理图片…', 'busy');
      try {
        const data = await shrinkImage(file);
        subjectBusy(card, false);
        subjectSend(card, {
          action: 'subject_upload',
          kind: card.dataset.kind,
          name: card.dataset.name,
          data
        }, '上传中…');
      } catch (err) {
        subjectBusy(card, false);
        subjectStatus(card, String(err.message || err), 'error');
      }
    }

    document.addEventListener('click', (e) => {
      const btn = e.target.closest('#tab-cast button[data-act]');
      if (!btn) return;
      const card = btn.closest('.subject');
      if (!card) return;
      const act = btn.dataset.act;
      if (act === 'upload') {
        const input = card.querySelector('input[type=file]');
        if (input) { input.value = ''; input.click(); }
      } else if (act === 'save') {
        saveSubject(card, false);
      } else if (act === 'regen') {
        saveSubject(card, true);
      } else if (act === 'reverse') {
        subjectSend(card, {
          action: 'subject_reverse', kind: card.dataset.kind, name: card.dataset.name
        }, '反推中…');
      } else if (act === 'clear') {
        clearSubject(card);
      }
    });

    document.addEventListener('change', (e) => {
      const input = e.target.closest ? e.target.closest('#tab-cast .subject input[type=file]') : null;
      if (!input) return;
      const card = input.closest('.subject');
      const file = (input.files || [])[0];
      if (card && file) uploadSubject(card, file);
    });

    function assetCard(a) {
      const isVideo = a.kind === 'video';
      const media = isVideo
        ? `<video src="${esc(a.url)}" controls preload="metadata"></video>`
        : `<img loading="lazy" data-full="${esc(a.url)}" src="${esc(a.url)}" alt="${esc(a.kind || '')}">`;
      const when = (a.created_at || '').replace('T', ' ').slice(0, 19);
      const del = a.id
        ? `<button class="asset-del" data-del="${esc(a.id)}" title="删除素材（含文件，并从角色参考中移除）">✕</button>`
        : '';
      return `<div class="asset">${del}${media}<div class="meta"><span class="kind">${esc(a.kind || '')}</span><span>${esc(when)}</span></div></div>`;
    }

    // 物料 deletion is optimistic: drop the card at once, then let the snapshot
    // confirm. The server unlinks the file and detaches the path from every
    // subject reference, so a deleted sheet also stops conditioning new frames.
    document.addEventListener('click', (e) => {
      const btn = e.target.closest('#asset-grid [data-del]');
      if (!btn) return;
      e.stopPropagation();
      const id = btn.getAttribute('data-del');
      if (!id) return;
      if (!window.confirm('删除这个素材？文件会一并删除，且不可撤销。')) return;
      if (!state.ws || state.ws.readyState !== 1) {
        log('socket 未连接，刷新页面后重试', 'error');
        return;
      }
      state.snap = state.snap || {};
      state.snap.assets = (state.snap.assets || []).filter((a) => a.id !== id);
      renderAssets();
      state.ws.send(JSON.stringify({ action: 'asset_delete', asset_id: id }));
      log('删除素材…', 'started');
    });

    function renderAssets() {
      const assets = (state.snap && state.snap.assets) || [];
      el('asset-grid').innerHTML = assets.length
        ? assets.map(assetCard).join('')
        : '<p class="empty">No assets yet.</p>';
    }

    function openLightbox(url, kind) {
      const body = el('lightbox-body');
      body.innerHTML = kind === 'video'
        ? `<video src="${esc(url)}" controls autoplay></video>`
        : `<img src="${esc(url)}">`;
      el('lightbox').classList.add('on');
    }

    document.addEventListener('click', (e) => {
      const node = e.target.closest('[data-full]');
      if (node) openLightbox(node.getAttribute('data-full'), 'image');
    });

    function renderOutputs() {
      const outs = (state.snap && state.snap.outputs) || [];
      el('outputs').innerHTML = outs.length
        ? outs.map((o) => `<li><a class="nav-link" href="${esc(o.url)}">${esc(o.name)}</a><span>${fmtSize(o.size)}</span></li>`).join('')
        : '<li class="empty">No outputs yet.</li>';
    }

    function renderAll() {
      renderSubtitle(); renderRail(); renderStagePick(); renderShots(); renderInspector(); renderOutputs(); renderCast(); renderAssets();
    }

    async function load() {
      try {
        const snap = await api('/toonflow/api/projects/' + encodeURIComponent(project));
        state.snap = snap; state.shots = snap.shots || []; state.cast = snap.cast || [];
        renderAll();
      } catch (err) {
        log('load failed: ' + err.message, 'error');
      }
    }

    // A shot event opens, refreshes or closes the live badge. A clip reports
    // `running` once per poll tick (~10s), so the log records the transition and
    // then at most one line every 30s — the badge carries the elapsed time.
    let liveLogAt = 0;
    function onShotEvent(msg) {
      const detail = msg.detail || {};
      const status = msg.status;
      const label = `  ${msg.stage} ${msg.shot_id}: `;

      if (status === 'started') {
        markLive(msg.shot_id, msg.stage, 'started');
        log(label + 'started', 'started');
      } else if (status === 'running') {
        const prev = markLive(msg.shot_id, msg.stage, detail.status || 'running');
        const current = state.live[msg.shot_id].status;
        const changed = !prev || prev.stage !== msg.stage || prev.status !== current;
        const now = Date.now();
        if (changed || now - liveLogAt > 30000) {
          liveLogAt = now;
          const secs = detail.elapsed_s != null ? detail.elapsed_s : (now - state.live[msg.shot_id].at) / 1000;
          // `step` names what a multi-step render is doing (a keyframe's end
          // frame); a plain heartbeat has none.
          const step = detail.step ? ' ' + detail.step : '';
          log(label + current + step + ' · ' + fmtClock(secs), 'started');
        }
      } else {
        clearLive(msg.shot_id);
        // A skip is a render with nothing to do (a shot without dialogue has no
        // voice) — say why instead of dumping the raw detail map.
        if (detail.skipped) {
          log(label + 'skipped (' + detail.skipped + ')', status);
        } else {
          log(label + status + (msg.detail ? ' ' + JSON.stringify(msg.detail) : ''), status);
        }
        // Fresh asset URLs only arrive in a snapshot, so ask for one now.
        requestRefresh();
      }
      paintLive();
    }

    // A badge left over from a tab that missed the `ok` event would tick
    // forever; snapshots arrive on every reconnect, so prune the old ones.
    function pruneLive() {
      const cutoff = Date.now() - 20 * 60 * 1000;
      Object.keys(state.live).forEach((id) => {
        if (state.live[id].at < cutoff) delete state.live[id];
      });
    }

    function setConn(on) {
      const c = el('conn');
      c.textContent = on ? 'connected' : 'disconnected';
      c.className = 'conn' + (on ? ' on' : '');
    }

    function connect() {
      const proto = location.protocol === 'https:' ? 'wss://' : 'ws://';
      const ws = new WebSocket(proto + location.host + '/toonflow/ws?project=' + encodeURIComponent(project));
      state.ws = ws;
      ws.onopen = () => { state.retries = 0; setConn(true); log('socket connected', 'ok'); };
      ws.onclose = () => {
        setConn(false);
        state.retries = Math.min(state.retries + 1, 6);
        setTimeout(connect, 1000 * state.retries);
      };
      ws.onerror = () => { /* onclose will fire */ };
      ws.onmessage = (ev) => {
        let msg = null;
        try { msg = JSON.parse(ev.data); } catch (_) { return; }
        if (!msg || !msg.type) return;
        if (msg.type === 'snapshot') {
          state.snap = msg; state.shots = msg.shots || []; state.cast = msg.cast || [];
          pruneLive();
          renderAll(); log('snapshot updated'); return;
        }
        if (msg.type === 'jobs' || msg.type === 'heartbeat') return;
        if (msg.type === 'pong') return;
        if (msg.type === 'error') { log('server error: ' + (msg.error || ''), 'error'); return; }
        if (msg.type === 'stage') { log(`stage ${msg.stage}: ${msg.status}` + (msg.detail ? ' ' + JSON.stringify(msg.detail) : ''), msg.status); return; }
        if (msg.type === 'shot') { onShotEvent(msg); return; }
        if (msg.type === 'subject') { onSubjectEvent(msg); return; }
        if (msg.type === 'assets') {
          if (msg.status === 'error') log('删除失败：' + (msg.error || ''), 'error');
          else log('已删除素材', 'ok');
          requestRefresh();
          return;
        }
        if (msg.type === 'job') { log(`job ${msg.status}`, msg.status); return; }
      };
    }

    async function run() {
      const btn = el('run'); btn.disabled = true;
      const msg = el('run-msg'); msg.className = 'msg'; msg.textContent = 'running…';
      const stages = Array.from(document.querySelectorAll('.stage-box:checked')).map((b) => b.value);
      const path = el('novel-path').value.trim();
      const payload = {
        stages,
        resume: el('opt-resume').checked,
        continue_on_error: el('opt-continue').checked
      };
      if (path) payload.path = path;
      try {
        const data = await api('/toonflow/api/projects/' + encodeURIComponent(project) + '/run', {
          method: 'POST', headers: { 'content-type': 'application/json' }, body: JSON.stringify(payload)
        });
        const ok = data && data.success !== false;
        msg.className = 'msg ' + (ok ? 'ok' : 'error');
        msg.textContent = ok ? 'pipeline finished' : ('pipeline failed: ' + JSON.stringify(data && (data.reason || data.error)));
        log('run result: ' + JSON.stringify(data && (data.completed != null ? { completed: data.completed } : data)), ok ? 'ok' : 'error');
      } catch (err) {
        msg.className = 'msg error'; msg.textContent = String(err.message || err);
      } finally {
        btn.disabled = false;
        load();
      }
    }

    el('run').addEventListener('click', run);
    el('refresh').addEventListener('click', load);
    load();
    connect();
    """
  end

  # ── helpers ──────────────────────────────────────────────────────────

  defp h(nil), do: ""

  defp h(value),
    do:
      value
      |> to_string()
      |> String.replace(["&", "<", ">", "\"", "'"], fn c -> escape_char(c) end)

  defp escape_char("&"), do: "&amp;"
  defp escape_char("<"), do: "&lt;"
  defp escape_char(">"), do: "&gt;"
  defp escape_char("\""), do: "&quot;"
  defp escape_char("'"), do: "&#39;"
end
