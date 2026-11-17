defmodule Exhub.Gitee.DashboardTest do
  use ExUnit.Case, async: true

  alias Exhub.Gitee.Dashboard

  describe "shape_dashboard/2" do
    test "keeps the sections the elisp renderer reads" do
      payload =
        Dashboard.shape_dashboard(1, %{
          enterprise: %{"name" => "开源中国", "path" => "oschina"},
          overview: %{"month_code_count" => 26_466},
          mine: %{"today_code_count" => 4_359},
          committers: [%{"login" => "yashin"}],
          projects: %{"total_count" => 48, "data" => []},
          programs: %{"total_count" => 7},
          members: %{"total_count" => 412},
          program_list: %{
            "total_count" => 7,
            "data" => [%{"id" => 5_080_111, "ident" => "P1093", "name" => "Gitee SaaS"}]
          },
          project_list: %{
            "total_count" => 4657,
            "data" => [%{"id" => 2, "path_with_namespace" => "oschina/gitee"}]
          },
          pulls: %{"total_count" => 3, "data" => [%{"iid" => 1}]},
          issues: %{"total_count" => 1224, "data" => [%{"ident" => "AI-01"}]}
        })

      assert payload["enterprise_id"] == 1
      assert payload["enterprise"] == %{"name" => "开源中国", "path" => "oschina"}
      assert payload["overview"] == %{"month_code_count" => 26_466}
      assert payload["mine"] == %{"today_code_count" => 4_359}
      assert payload["committers"] == [%{"login" => "yashin"}]
      assert payload["projects"] == 48
      assert payload["programs"] == 7
      assert payload["members"] == 412

      assert payload["program_items"] == [
               %{"id" => 5_080_111, "ident" => "P1093", "name" => "Gitee SaaS"}
             ]

      assert payload["project_items"] == [
               %{"id" => 2, "path_with_namespace" => "oschina/gitee"}
             ]

      assert payload["pulls"] == [%{"iid" => 1}]
      assert payload["pulls_total"] == 3
      assert payload["issues"] == [%{"ident" => "AI-01"}]
      assert payload["issues_total"] == 1224
      assert payload["errors"] == %{}
      assert is_binary(payload["generated_at"])
    end

    test "degrades a failed section to an empty value and records the error" do
      payload =
        Dashboard.shape_dashboard(1, %{
          enterprise: %{"name" => "开源中国"},
          overview: {:error, {:http, 500, "boom"}},
          committers: {:error, {:transport, "timeout"}},
          projects: {:error, {:http, 500, "boom"}},
          program_list: {:error, {:http, 500, "boom"}},
          project_list: {:error, {:transport, "timeout"}},
          pulls: {:error, {:http, 429, "slow down"}},
          issues: %{"total_count" => 0, "data" => []}
        })

      assert payload["overview"] == %{}
      assert payload["committers"] == []
      assert payload["projects"] == nil
      assert payload["program_items"] == []
      assert payload["project_items"] == []
      assert payload["pulls"] == []
      assert payload["pulls_total"] == nil
      assert payload["issues"] == []

      assert payload["errors"] |> Map.keys() |> Enum.sort() ==
               ~w(committers overview program_list project_list projects pulls) |> Enum.sort()
    end

    test "ignores a non-list committer ranking" do
      payload = Dashboard.shape_dashboard(1, %{committers: %{"message" => "nope"}})
      assert payload["committers"] == []
    end
  end

  describe "shape_program/1" do
    test "keeps the program, its repositories and its unfinished issues" do
      payload =
        Dashboard.shape_program(%{
          program: %{"id" => 5_080_111, "ident" => "P1093", "name" => "Gitee SaaS"},
          projects: %{
            "total_count" => 7,
            "data" => [%{"id" => 10_190_713, "path_with_namespace" => "oschina/gitee-ent-web"}]
          },
          issues: %{
            "total_count" => 42,
            "data" => [%{"ident" => "AI-01", "title" => "Crash"}]
          }
        })

      assert payload["program"]["name"] == "Gitee SaaS"

      assert payload["projects"] == [
               %{"id" => 10_190_713, "path_with_namespace" => "oschina/gitee-ent-web"}
             ]

      assert payload["projects_total"] == 7
      assert payload["issues"] == [%{"ident" => "AI-01", "title" => "Crash"}]
      assert payload["issues_total"] == 42
      assert payload["errors"] == %{}
      assert is_binary(payload["generated_at"])
    end

    test "degrades failed sections but keeps the program" do
      payload =
        Dashboard.shape_program(%{
          program: %{"id" => 1, "name" => "One"},
          projects: {:error, {:http, 500, "boom"}},
          issues: {:error, {:transport, "timeout"}}
        })

      assert payload["program"]["name"] == "One"
      assert payload["projects"] == []
      assert payload["projects_total"] == nil
      assert payload["issues"] == []
      assert payload["issues_total"] == nil

      assert payload["errors"] == %{
               "projects" => "Gitee API returned HTTP 500: boom",
               "issues" => "cannot reach the Gitee API: timeout"
             }
    end
  end

  describe "shape_detail/3" do
    test "pull request timeline uses notes.list and counts operate logs" do
      payload =
        Dashboard.shape_detail("pull", "42", %{
          detail: %{"title" => "Fix it", "iid" => 42},
          notes: %{
            "count" => 3,
            "list" => [
              %{"type" => "note", "content" => "hi"},
              %{"type" => "operate_log", "content" => "changed assignee"},
              %{"type" => "operate_log", "content" => "approved"}
            ]
          }
        })

      assert payload["kind"] == "pull"
      assert payload["ref"] == "42"
      assert payload["detail"] == %{"title" => "Fix it", "iid" => 42}
      assert length(payload["timeline"]) == 3
      assert payload["timeline_total"] == 5
    end

    test "issue timeline merges comments and events" do
      payload =
        Dashboard.shape_detail("issue", "AI-01", %{
          detail: %{"title" => "Crash"},
          notes: %{"total_count" => 2, "data" => [%{"content" => "a"}, %{"content" => "b"}]},
          logs: %{"total_count" => 1, "data" => [%{"content" => "assigned"}]}
        })

      assert payload["kind"] == "issue"
      assert Enum.map(payload["timeline"], & &1["content"]) == ["a", "b", "assigned"]
      assert payload["timeline_total"] == 3
    end

    test "a missing timeline section degrades to zero entries" do
      payload = Dashboard.shape_detail("issue", "AI-02", %{detail: %{}, notes: {:error, :boom}})

      assert payload["timeline"] == []
      assert payload["timeline_total"] == 0
      assert payload["errors"] == %{"notes" => ":boom"}
    end
  end

  describe "error_message/1" do
    test "names the SecretVault key when the token is missing" do
      message = Dashboard.error_message(:missing_token)
      assert message =~ "gitee_ent_mcp_access_token"
      assert message =~ "exhub_reload_keys"
    end

    test "describes an upstream failure with its section" do
      assert Dashboard.error_message({:section, :enterprise, {:http, 411, "登录已失效"}}) ==
               "enterprise: Gitee API returned HTTP 411: 登录已失效"
    end
  end

  describe "dashboard/1 without a token" do
    test "returns :missing_token instead of touching the network" do
      if :persistent_term.get("gitee_ent_mcp_access_token", nil) == nil and
           Application.get_env(:exhub, :gitee_ent_mcp_access_token) == nil do
        assert Dashboard.dashboard() == {:error, :missing_token}
      end
    end
  end
end
