defmodule GiteeCat.Dashboard do
  @moduledoc """
  Gitee Enterprise dashboard, statistics and detail endpoints.

  Thin path wrappers over `GiteeCat.get/4`, in the same shape as
  `GiteeCat.Issues` and `GiteeCat.Pulls`. Every call passes
  `pagination: :none`: a dashboard section is a single page, and following
  `Link` headers would silently turn one request into several.

  See `~/Code/gitee/app/api/enterprise_v1/` for the upstream definitions.
  """

  import GiteeCat
  alias GiteeCat.Client

  @one_page [pagination: :none]

  @doc "Enterprise profile: `GET /enterprises/<id>`."
  @spec enterprise(Client.t(), pos_integer()) :: GiteeCat.response()
  def enterprise(client, ent_id) do
    get("enterprises/#{ent_id}", client, [], @one_page)
  end

  @doc "Organisation overview statistics: `GET .../dashboard_statistics/overview`."
  @spec overview(Client.t(), pos_integer()) :: GiteeCat.response()
  def overview(client, ent_id) do
    get("enterprises/#{ent_id}/dashboard_statistics/overview", client, [], @one_page)
  end

  @doc "The current user's statistics: `GET .../dashboard_statistics/user_dashboard`."
  @spec user_dashboard(Client.t(), pos_integer()) :: GiteeCat.response()
  def user_dashboard(client, ent_id) do
    get("enterprises/#{ent_id}/dashboard_statistics/user_dashboard", client, [], @one_page)
  end

  @doc "Monthly commit ranking: `GET .../statistics/top_committer?rank=<rank>`."
  @spec top_committer(Client.t(), pos_integer(), pos_integer()) :: GiteeCat.response()
  def top_committer(client, ent_id, rank) do
    get("enterprises/#{ent_id}/statistics/top_committer", client, [rank: rank], @one_page)
  end

  @doc """
  Total count of an enterprise collection (`:projects`, `:programs`, `:members`).

  Requests a single row and lets the caller read `total_count`.
  """
  @spec count(Client.t(), pos_integer(), :projects | :programs | :members) ::
          GiteeCat.response()
  def count(client, ent_id, collection) when collection in [:projects, :programs, :members] do
    get("enterprises/#{ent_id}/#{collection}", client, [per_page: 1], @one_page)
  end

  @doc "Pull requests related to the current user: `GET .../pull_requests`."
  @spec pulls(Client.t(), pos_integer(), keyword()) :: GiteeCat.response()
  def pulls(client, ent_id, params \\ []) do
    get("enterprises/#{ent_id}/pull_requests", client, params, @one_page)
  end

  @doc "Open issues related to the current user: `GET .../issues`."
  @spec issues(Client.t(), pos_integer(), keyword()) :: GiteeCat.response()
  def issues(client, ent_id, params \\ []) do
    get("enterprises/#{ent_id}/issues", client, params, @one_page)
  end

  @doc """
  One pull request: `GET .../projects/<project_id>/pull_requests/<iid>?pr_qt=iid`.

  The project segment only accepts the numeric project id.
  """
  @spec pull(Client.t(), pos_integer(), pos_integer() | binary(), pos_integer() | binary()) ::
          GiteeCat.response()
  def pull(client, ent_id, project_id, iid) do
    get(
      "enterprises/#{ent_id}/projects/#{project_id}/pull_requests/#{iid}",
      client,
      [pr_qt: "iid"],
      @one_page
    )
  end

  @doc "Timeline trees of a pull request: `GET .../pull_requests/<iid>/notes/trees`."
  @spec pull_timeline(
          Client.t(),
          pos_integer(),
          pos_integer() | binary(),
          pos_integer() | binary()
        ) ::
          GiteeCat.response()
  def pull_timeline(client, ent_id, project_id, iid) do
    get(
      "enterprises/#{ent_id}/projects/#{project_id}/pull_requests/#{iid}/notes/trees",
      client,
      [pr_qt: "iid", direction: "asc"],
      @one_page
    )
  end

  @doc "One issue: `GET .../issues/<ident>?qt=ident`."
  @spec issue(Client.t(), pos_integer(), binary()) :: GiteeCat.response()
  def issue(client, ent_id, ident) do
    get("enterprises/#{ent_id}/issues/#{ident}", client, [qt: "ident"], @one_page)
  end

  @doc "Issue comments: `GET .../issues/<ident>/notes?qt=ident`."
  @spec issue_notes(Client.t(), pos_integer(), binary(), pos_integer()) :: GiteeCat.response()
  def issue_notes(client, ent_id, ident, per_page) do
    get(
      "enterprises/#{ent_id}/issues/#{ident}/notes",
      client,
      [qt: "ident", per_page: per_page],
      @one_page
    )
  end

  @doc "Issue operation log (events): `GET .../issues/<ident>/operate_logs?qt=ident`."
  @spec issue_logs(Client.t(), pos_integer(), binary(), pos_integer()) :: GiteeCat.response()
  def issue_logs(client, ent_id, ident, limit) do
    get(
      "enterprises/#{ent_id}/issues/#{ident}/operate_logs",
      client,
      [qt: "ident", limit: limit],
      @one_page
    )
  end
end
