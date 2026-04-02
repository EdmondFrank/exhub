defmodule Exhub.MCP.Archery.Client do
  @moduledoc """
  HTTP client for interacting with the Archery SQL audit platform.
  Archery uses Django session-based authentication with CSRF protection.
  """

  require Logger

  @type t :: %__MODULE__{
    base_url: String.t(),
    username: String.t(),
    password: String.t(),
    authenticated: boolean(),
    cookie_jar: map()
  }

  defstruct [:base_url, :username, :password, :authenticated, :cookie_jar]

  @doc """
  Creates a new Archery client from environment configuration.
  """
  @spec new() :: t()
  def new do
    base_url = get_config(:archery_url, "")
    username = get_config(:archery_username, "")
    password = get_config(:archery_password, "")

    %__MODULE__{
      base_url: base_url,
      username: username,
      password: password,
      authenticated: false,
      cookie_jar: %{}
    }
  end

  @doc """
  Returns true if the client has valid configuration.
  """
  @spec valid_config?(t()) :: boolean()
  def valid_config?(%__MODULE__{base_url: url, username: user, password: pass}) do
    url != "" and user != "" and pass != ""
  end

  @doc """
  Authenticates with Archery using Django session-based auth.
  """
  @spec login(t()) :: {:ok, t()} | {:error, String.t()}
  def login(%__MODULE__{} = client) do
    unless valid_config?(client) do
      Logger.error("[Archery] Login failed: invalid config")
      return {:error, "Archery configuration is incomplete. Please set ARCHERY_URL, ARCHERY_USERNAME, ARCHERY_PASSWORD"}
    end

    login_page_url = "#{client.base_url}/login/"

    # Step 1: GET login page to obtain CSRF token
    case http_get(login_page_url, %{}, %{}) do
      {:ok, %{status: 200, body: body, cookies: cookies}} ->
        csrf_token = extract_csrf_token(cookies, body)

        if csrf_token == "" do
          Logger.error("[Archery] Could not extract CSRF token from cookies or body")
          return {:error, "Could not obtain CSRF token"}
        end

        # Step 2: POST login form
        login_url = "#{client.base_url}/authenticate/"
        form_data = %{
          "username" => client.username,
          "password" => client.password,
          "csrfmiddlewaretoken" => csrf_token
        }

        headers = %{
          "Content-Type" => "application/x-www-form-urlencoded",
          "Referer" => login_page_url
        }

        case http_post_form(login_url, form_data, headers, cookies) do
          {:ok, %{status: status, redirect_url: redirect_url, cookies: new_cookies}}
          when status in [200, 302] ->
            if String.contains?(String.downcase(redirect_url), "login") do
              Logger.warning("[Archery] Login redirected back to login page, trying API login fallback")
              login_api(client)
            else
              {:ok, %{client | authenticated: true, cookie_jar: new_cookies}}
            end

          {:ok, %{status: status, body: body}} ->
            Logger.error("[Archery] Login failed with status #{status}, body: #{String.slice(body, 0, 200)}")
            {:error, "Login failed with status #{status}"}

          {:error, reason} ->
            Logger.error("[Archery] Login request failed: #{inspect(reason)}")
            {:error, "Login request failed: #{inspect(reason)}"}
        end

      {:ok, %{status: status}} ->
        Logger.error("[Archery] Login page returned status #{status}")
        {:error, "Login page request failed with status #{status}"}

      {:error, reason} ->
        Logger.error("[Archery] Login page request failed: #{inspect(reason)}")
        {:error, "Login page request failed: #{inspect(reason)}"}
    end
  end

  defp login_api(%__MODULE__{} = client) do
    api_url = "#{client.base_url}/api/v1/auth/token/"
    payload = Jason.encode!(%{"username" => client.username, "password" => client.password})

    case http_post_json(api_url, payload, %{}, %{}) do
      {:ok, %{status: 200, body: body}} ->
        case Jason.decode(body) do
          {:ok, data} ->
            token = data["token"] || data["access"]
            if token do
              {:ok, %{client | authenticated: true, cookie_jar: %{"token" => token}}}
            else
              {:error, "API login failed: no token in response"}
            end
          {:error, _} ->
            {:error, "API login failed: invalid response"}
        end

      {:ok, %{status: status}} ->
        {:error, "API login failed with status #{status}"}

      {:error, reason} ->
        {:error, "API login request failed: #{inspect(reason)}"}
    end
  end

  @doc """
  Returns a list of database instances, optionally filtered by db_type.
  """
  @spec get_instances(t(), String.t()) :: {:ok, [map()], t()} | {:error, String.t(), t()}
  def get_instances(%__MODULE__{} = client, db_type \\ "") do
    with {:ok, authenticated_client} <- ensure_authenticated(client),
         params <- if(db_type != "", do: %{"db_type" => db_type}, else: %{}),
         {:ok, %{status: 200, body: body}, updated_client} <-
           api_request(authenticated_client, :get, "/api/v1/instance/", nil, params) do
      case Jason.decode(body) do
        {:ok, result} when is_list(result) ->
          {:ok, result, updated_client}

        {:ok, result} ->
          instances = extract_list(result, ["data", "results"])
          {:ok, instances, updated_client}

        {:error, reason} ->
          Logger.warning("[Archery] JSON decode failed for instance API: #{reason}, falling back to query log")
          get_instances_from_querylog(updated_client)
      end
    else
      {:ok, %{status: _status, body: _body}, updated_client} ->
        Logger.warning("[Archery] Instance API returned non-200, falling back to query log")
        get_instances_from_querylog(updated_client)
      {:error, _reason, updated_client} ->
        Logger.warning("[Archery] Error getting instances, falling back to query log")
        get_instances_from_querylog(updated_client)
    end
  end

  defp get_instances_from_querylog(%__MODULE__{} = client) do
    req_url = "#{client.base_url}/query/querylog/?limit=500&offset=0"

    headers = %{
      "X-CSRFToken" => get_csrf_token(client),
      "X-Requested-With" => "XMLHttpRequest",
      "Referer" => "#{client.base_url}/sqlquery/"
    }

    case http_get(req_url, headers, client.cookie_jar) do
      {:ok, %{status: 200, body: body, cookies: new_cookies}} ->
        updated_client = %{client | cookie_jar: Map.merge(client.cookie_jar, new_cookies)}
        case Jason.decode(body) do
          {:ok, data} ->
            rows = data["rows"] || []
            instances = extract_instances_from_rows(rows)
            {:ok, instances, updated_client}
          {:error, reason} ->
            Logger.error("[Archery] Failed to decode querylog response: #{reason}")
            {:ok, [], updated_client}
        end
      {:ok, %{status: status, body: body, cookies: new_cookies}} ->
        updated_client = %{client | cookie_jar: Map.merge(client.cookie_jar, new_cookies)}
        Logger.warning("[Archery] Querylog returned status #{status}, body: #{String.slice(body, 0, 200)}")
        {:ok, [], updated_client}
      {:error, reason} ->
        Logger.error("[Archery] Querylog request failed: #{inspect(reason)}")
        {:ok, [], client}
    end
  end

  defp extract_instances_from_rows(rows) when is_list(rows) do
    rows
    |> Enum.reduce(%{}, fn row, acc ->
      inst_name = row["instance_name"]
      db_name = row["db_name"]

      if inst_name do
        Map.update(acc, inst_name, %{"instance_name" => inst_name, "databases" => []}, fn existing ->
          dbs = existing["databases"] || []
          if db_name && db_name not in dbs do
            %{existing | "databases" => [db_name | dbs]}
          else
            existing
          end
        end)
      else
        acc
      end
    end)
    |> Map.values()
  end

  @doc """
  Returns the list of databases for the given instance.
  """
  @spec get_databases(t(), String.t()) :: {:ok, [String.t()], t()} | {:error, String.t(), t()}
  def get_databases(%__MODULE__{} = client, instance_name) do
    with {:ok, authenticated_client} <- ensure_authenticated(client),
         {:ok, %{status: 200, body: body}, updated_client} <-
           api_request(authenticated_client, :get, "/api/v1/instance/databases/", nil, %{"instance_name" => instance_name}) do
      case Jason.decode(body) do
        {:ok, result} ->
          dbs = case result["data"] do
            list when is_list(list) -> list
            _ -> extract_list(result, ["databases"])
          end
          {:ok, Enum.map(dbs, &to_string/1), updated_client}

        {:error, _} ->
          {:ok, [], updated_client}
      end
    else
      {:ok, %{status: _status, body: _body}, updated_client} ->
        Logger.warning("[Archery] get_databases API failed, falling back to querylog")
        get_databases_from_querylog(updated_client, instance_name)
      {:error, _reason, updated_client} ->
        Logger.warning("[Archery] get_databases error, falling back to querylog")
        get_databases_from_querylog(updated_client, instance_name)
    end
  end

  defp get_databases_from_querylog(%__MODULE__{} = client, instance_name) do
    with {:ok, instances, updated_client} <- get_instances_from_querylog(client) do
      dbs =
        instances
        |> Enum.find(%{}, fn inst -> inst["instance_name"] == instance_name end)
        |> Map.get("databases", [])
        |> Enum.map(&to_string/1)
      {:ok, dbs, updated_client}
    end
  end

  @doc """
  Executes a read-only SQL query (SELECT only).
  """
  @spec query_execute(t(), String.t(), String.t(), String.t(), integer()) :: {:ok, map(), t()} | {:error, String.t(), t()}
  def query_execute(%__MODULE__{} = client, sql_content, instance_name, db_name, limit \\ 100) do
    with {:ok, client} <- ensure_authenticated(client) do
      sql_stripped = String.trim(sql_content)
      sql_upper = String.upcase(sql_stripped)

      # Security check: only allow SELECT and safe operations
      dangerous = ["DROP", "DELETE", "TRUNCATE", "UPDATE", "INSERT", "ALTER", "CREATE"]
      is_mongodb = String.starts_with?(sql_stripped, "db.")
      is_redis = Enum.any?(["GET ", "SET ", "KEYS ", "HGET", "SCAN "], fn cmd ->
        String.starts_with?(sql_upper, cmd)
      end)

      unless is_mongodb or is_redis do
        if Enum.any?(dangerous, fn kw -> String.starts_with?(sql_upper, kw) end) do
          return {:error, "Dangerous operation not allowed. For DDL/DML operations, please use submit_workflow", client}
        end
      end

      # Auto-append LIMIT for SELECT queries
      sql_content = if String.starts_with?(sql_upper, "SELECT") and not String.contains?(sql_upper, "LIMIT") do
        String.trim_trailing(sql_stripped, ";") <> " LIMIT #{limit}"
      else
        sql_content
      end

      query_url = "#{client.base_url}/query/"
      form_data = %{
        "instance_name" => instance_name,
        "db_name" => db_name,
        "sql_content" => sql_content,
        "limit_num" => to_string(limit)
      }

      headers = %{
        "Content-Type" => "application/x-www-form-urlencoded",
        "X-CSRFToken" => get_csrf_token(client),
        "X-Requested-With" => "XMLHttpRequest",
        "Referer" => "#{client.base_url}/sqlquery/"
      }

      case http_post_form(query_url, form_data, headers, client.cookie_jar) do
        {:ok, %{status: 200, body: body}} ->
          case Jason.decode(body) do
            {:ok, result} ->
              status = result["status"]
              if status == 0 do
                data = result["data"] || %{}
                {:ok, data, client}
              else
                {:error, result["msg"] || "Query failed", client}
              end
            {:error, _} ->
              {:error, "Failed to parse query response", client}
          end

        {:ok, %{status: status}} ->
          {:error, "Query failed with status #{status}", client}

        {:error, reason} ->
          {:error, "Query request failed: #{inspect(reason)}", client}
      end
    end
  end

  @doc """
  Validates SQL syntax and returns optimization suggestions.
  """
  @spec sql_check(t(), String.t(), String.t(), String.t()) :: {:ok, map(), t()} | {:error, String.t(), t()}
  def sql_check(%__MODULE__{} = client, sql_content, instance_name, db_name) do
    with {:ok, authenticated_client} <- ensure_authenticated(client) do
      api_post(authenticated_client, "/api/v1/sql/check/", %{
        "sql_content" => sql_content,
        "instance_name" => instance_name,
        "db_name" => db_name
      })
    end
  end

  @doc """
  Submits SQL for audit review (creates a workflow/ticket).
  """
  @spec sql_review(t(), String.t(), String.t(), String.t(), String.t()) :: {:ok, map(), t()} | {:error, String.t(), t()}
  def sql_review(%__MODULE__{} = client, sql_content, instance_name, db_name, workflow_name \\ "") do
    data = %{
      "sql_content" => sql_content,
      "instance_name" => instance_name,
      "db_name" => db_name
    }
    data = if workflow_name != "", do: Map.put(data, "workflow_name", workflow_name), else: data

    with {:ok, client} <- ensure_authenticated(client),
         {:ok, result, client} <- api_post(client, "/api/v1/sql/review/", data) do
      {:ok, result, client}
    else
      {:error, reason, client} -> {:error, reason, client}
    end
  end

  @doc """
  Returns SQL workflows filtered by optional parameters.
  """
  @spec get_workflow_list(t(), String.t(), String.t(), String.t(), integer()) :: {:ok, [map()], t()} | {:error, String.t(), t()}
  def get_workflow_list(%__MODULE__{} = client, status \\ "", start_date \\ "", end_date \\ "", limit \\ 50) do
    params = %{"limit" => to_string(limit)}
    params = if status != "", do: Map.put(params, "status", status), else: params
    params = if start_date != "", do: Map.put(params, "start_date", start_date), else: params
    params = if end_date != "", do: Map.put(params, "end_date", end_date), else: params

    with {:ok, authenticated_client} <- ensure_authenticated(client),
         {:ok, result, updated_client} <- api_get(authenticated_client, "/api/v1/workflow/", params) do
      workflows = extract_list(result, ["data", "results"])
      {:ok, workflows, updated_client}
    end
  end

  @doc """
  Returns details for a specific workflow.
  """
  @spec get_workflow_detail(t(), integer()) :: {:ok, map(), t()} | {:error, String.t(), t()}
  def get_workflow_detail(%__MODULE__{} = client, workflow_id) do
    with {:ok, authenticated_client} <- ensure_authenticated(client) do
      api_get(authenticated_client, "/api/v1/workflow/#{workflow_id}/", %{})
    end
  end

  @doc """
  Returns query execution history.
  """
  @spec get_query_history(t(), String.t(), String.t(), integer()) :: {:ok, [map()], t()} | {:error, String.t(), t()}
  def get_query_history(%__MODULE__{} = client, instance_name \\ "", db_name \\ "", limit \\ 50) do
    with {:ok, authenticated_client} <- ensure_authenticated(client) do
      params = %{"limit" => to_string(limit), "offset" => "0"}
      params = if instance_name != "", do: Map.put(params, "instance_name", instance_name), else: params
      params = if db_name != "", do: Map.put(params, "db_name", db_name), else: params

      query_string = URI.encode_query(params)
      url = "#{authenticated_client.base_url}/query/querylog/?#{query_string}"

      headers = %{
        "X-Requested-With" => "XMLHttpRequest",
        "X-CSRFToken" => get_csrf_token(authenticated_client),
        "Referer" => "#{authenticated_client.base_url}/sqlquery/"
      }

      case http_get(url, headers, authenticated_client.cookie_jar) do
        {:ok, %{status: 200, body: body, cookies: new_cookies}} ->
          updated_client = %{authenticated_client | cookie_jar: Map.merge(authenticated_client.cookie_jar, new_cookies)}
          case Jason.decode(body) do
            {:ok, data} ->
              rows = data["rows"] || []
              history = Enum.map(rows, fn row ->
                %{
                  "instance_name" => row["instance_name"] || "",
                  "db_name" => row["db_name"] || "",
                  "sqllog" => row["sqllog"] || row["sql_content"] || "",
                  "effect_row" => row["effect_row"] || 0,
                  "cost_time" => row["cost_time"] || "",
                  "username" => row["username"] || "",
                  "create_time" => row["create_time"] || ""
                }
              end)
              {:ok, history, updated_client}
            {:error, reason} ->
              Logger.error("[Archery] Failed to decode querylog response: #{reason}")
              {:ok, [], updated_client}
          end

        {:ok, %{status: status, body: _body, cookies: new_cookies}} ->
          updated_client = %{authenticated_client | cookie_jar: Map.merge(authenticated_client.cookie_jar, new_cookies)}
          Logger.warning("[Archery] querylog returned status #{status}")
          {:ok, [], updated_client}

        {:error, reason} ->
          Logger.error("[Archery] querylog request failed: #{inspect(reason)}")
          {:ok, [], authenticated_client}
      end
    end
  end

  @doc """
  Returns all available resource group names.
  """
  @spec get_resource_groups(t()) :: {:ok, [String.t()], t()} | {:error, String.t(), t()}
  def get_resource_groups(%__MODULE__{} = client) do
    with {:ok, authenticated_client} <- ensure_authenticated(client),
         {:ok, %{status: 200, body: body}} <-
           http_get("#{authenticated_client.base_url}/submitsql/", %{}, authenticated_client.cookie_jar) do
      groups = parse_resource_groups_from_html(body)
      {:ok, groups, authenticated_client}
    else
      {:error, reason, client} -> {:error, reason, client}
      {:error, reason} -> {:error, inspect(reason), client}
    end
  end

  defp parse_resource_groups_from_html(html) do
    # Extract from select element with id="group_name"
    case Regex.run(~r/id="group_name"[^>]*>(.*?)<\/select>/s, html) do
      [_, select_content] ->
        Regex.scan(~r/value="([^"]+)"[^>]*>([^<]+)<\/option>/, select_content)
        |> Enum.filter(fn [_, val, _] -> val != "" and val != "is-empty" end)
        |> Enum.map(fn [_, _, name] -> String.trim(name) end)
      _ ->
        []
    end
  end

  @doc """
  Returns instances available under a resource group.
  """
  @spec get_group_instances(t(), String.t()) :: {:ok, [map()], t()} | {:error, String.t(), t()}
  def get_group_instances(%__MODULE__{} = client, group_name) do
    with {:ok, client} <- ensure_authenticated(client) do
      form_data = %{"group_name" => group_name}
      headers = %{
        "Content-Type" => "application/x-www-form-urlencoded",
        "X-Requested-With" => "XMLHttpRequest",
        "X-CSRFToken" => get_csrf_token(client)
      }

      case http_post_form("#{client.base_url}/group/instances/", form_data, headers, client.cookie_jar) do
        {:ok, %{status: 200, body: body}} ->
          case Jason.decode(body) do
            {:ok, data} ->
              status = data["status"]
              if status == 0 do
                instances = extract_list(data, ["data"])
                {:ok, instances, client}
              else
                {:error, "Failed to get instances for group #{group_name}", client}
              end
            {:error, _} ->
              {:error, "Failed to parse group instances response", client}
          end

        {:ok, %{status: status}} ->
          {:error, "Failed to get instances for group #{group_name}: status #{status}", client}

        {:error, reason} ->
          {:error, "Failed to get group instances: #{inspect(reason)}", client}
      end
    end
  end

  @doc """
  Validates SQL before submitting a workflow.
  """
  @spec check_sql_for_workflow(t(), integer(), String.t(), String.t()) :: {:ok, map(), t()} | {:error, String.t(), t()}
  def check_sql_for_workflow(%__MODULE__{} = client, instance_id, db_name, sql_content) do
    with {:ok, client} <- ensure_authenticated(client) do
      payload = Jason.encode!(%{
        "instance_id" => instance_id,
        "db_name" => db_name,
        "full_sql" => sql_content
      })

      headers = %{
        "Content-Type" => "application/json",
        "X-Requested-With" => "XMLHttpRequest",
        "X-CSRFToken" => get_csrf_token(client)
      }

      case http_post("#{client.base_url}/api/v1/workflow/sqlcheck/", payload, headers, client.cookie_jar) do
        {:ok, %{status: 200, body: body}} ->
          case Jason.decode(body) do
            {:ok, result} -> {:ok, result, client}
            {:error, _} -> {:ok, %{}, client}
          end

        {:ok, %{status: status, body: body}} ->
          {:error, "SQL check failed: status #{status} - #{String.slice(body, 0, 500)}", client}

        {:error, reason} ->
          {:error, "SQL check failed: #{inspect(reason)}", client}
      end
    end
  end

  @doc """
  Submits a SQL workflow for review/approval.
  """
  @spec submit_workflow(t(), String.t(), String.t(), String.t(), String.t(), String.t(), boolean(), String.t()) ::
          {:ok, map(), t()} | {:error, String.t(), t()}
  def submit_workflow(%__MODULE__{} = client, workflow_name, group_name, instance_name, db_name, sql_content, is_backup \\ true, demand_url \\ "") do
    with {:ok, client} <- ensure_authenticated(client),
         {:ok, instances, client} <- get_group_instances(client, group_name),
         {:ok, instance_id, client} <- resolve_instance_id(instances, instance_name, client),
         {:ok, group_id, client} <- get_group_id_from_page(client, group_name),
         {:ok, check_result, client} <- check_sql_for_workflow(client, instance_id, db_name, sql_content) do

      is_critical = check_result["is_critical"] || false
      if is_critical do
        {:error, "SQL check failed with critical errors: #{inspect(check_result)}", client}
      else
        # Try REST API first
        case submit_workflow_api(client, workflow_name, group_id, instance_id, db_name, sql_content, is_backup, demand_url) do
          {:ok, result, client} ->
            {:ok, result, client}
          {:error, _, client} ->
            # Fallback to form submission
            submit_workflow_form(client, workflow_name, group_name, group_id, instance_id, db_name, sql_content, is_backup, demand_url)
        end
      end
    else
      {:error, reason, client} -> {:error, reason, client}
    end
  end

  defp resolve_instance_id(instances, instance_name, client) do
    case Enum.find(instances, fn inst -> inst["instance_name"] == instance_name end) do
      nil ->
        {:error, "Instance '#{instance_name}' not found", client}
      inst ->
        id = trunc(inst["id"] || 0)
        if id > 0 do
          {:ok, id, client}
        else
          {:error, "Instance '#{instance_name}' has no valid ID", client}
        end
    end
  end

  defp get_group_id_from_page(%__MODULE__{} = client, group_name) do
    # Try to get group ID from the submitsql page
    with {:ok, %{status: 200, body: body}} <-
           http_get("#{client.base_url}/submitsql/", %{}, client.cookie_jar) do

      # Try pattern: value="X" group-id="N"
      patterns = [
        ~r/<option[^>]*value="([^"]+)"[^>]*group-id="(\d+)"[^>]*>([^<]*)<\/option>/,
        ~r/<option[^>]*group-id="(\d+)"[^>]*value="([^"]+)"[^>]*>([^<]*)<\/option>/
      ]

      group_id = Enum.find_value(patterns, fn pattern ->
        Regex.scan(pattern, body)
        |> Enum.find_value(fn match ->
          case match do
            [_, val, gid, text] when text == group_name or val == group_name ->
              String.to_integer(gid)
            [_, gid, val, text] when text == group_name or val == group_name ->
              String.to_integer(gid)
            _ -> nil
          end
        end)
      end)

      if group_id do
        {:ok, group_id, client}
      else
        # Fallback: try API
        get_group_id_from_api(client, group_name)
      end
    else
      {:error, reason} -> {:error, "Failed to get group ID: #{inspect(reason)}", client}
    end
  end

  defp get_group_id_from_api(%__MODULE__{} = client, group_name) do
    endpoints = ["/group/list/", "/group/group/"]

    Enum.find_value(endpoints, {:error, "Could not find group ID for '#{group_name}'", client}, fn endpoint ->
      case http_get("#{client.base_url}#{endpoint}", %{"X-Requested-With" => "XMLHttpRequest", "X-CSRFToken" => get_csrf_token(client)}, client.cookie_jar) do
        {:ok, %{status: 200, body: body}} ->
          case Jason.decode(body) do
            {:ok, data} ->
              rows = data["rows"] || data["data"] || data["results"] || []
              case Enum.find(rows, fn r -> r["group_name"] == group_name end) do
                nil -> nil
                row ->
                  gid = trunc(row["group_id"] || row["id"] || 0)
                  if gid > 0, do: {:ok, gid, client}, else: nil
              end
            {:error, _} -> nil
          end
        _ -> nil
      end
    end)
  end

  defp submit_workflow_api(%__MODULE__{} = client, workflow_name, group_id, instance_id, db_name, sql_content, is_backup, demand_url) do
    payload = Jason.encode!(%{
      "sql_content" => sql_content,
      "workflow" => %{
        "workflow_name" => workflow_name,
        "group_id" => group_id,
        "instance" => instance_id,
        "db_name" => db_name,
        "is_backup" => is_backup,
        "demand_url" => demand_url,
        "is_offline_export" => false
      }
    })

    headers = %{
      "Content-Type" => "application/json",
      "X-CSRFToken" => get_csrf_token(client),
      "Referer" => "#{client.base_url}/submitsql/"
    }

    case http_post("#{client.base_url}/api/v1/workflow/", payload, headers, client.cookie_jar) do
      {:ok, %{status: status, body: body}} when status in [200, 201] ->
        case Jason.decode(body) do
          {:ok, result} ->
            {:ok, %{"status" => 0, "msg" => "Workflow submitted successfully", "data" => result}, client}
          {:error, _} ->
            {:ok, %{"status" => 0, "msg" => "Workflow submitted successfully"}, client}
        end

      {:ok, %{status: status, body: body}} ->
        {:error, "API submission failed: #{status} - #{String.slice(body, 0, 500)}", client}

      {:error, reason} ->
        {:error, "API submission request failed: #{inspect(reason)}", client}
    end
  end

  defp submit_workflow_form(%__MODULE__{} = client, workflow_name, _group_name, group_id, instance_id, db_name, sql_content, is_backup, demand_url) do
    backup = if is_backup, do: "1", else: "0"

    form_data = %{
      "workflow_name" => workflow_name,
      "group_name" => to_string(group_id),
      "instance_name" => to_string(instance_id),
      "db_name" => db_name,
      "sql_content" => sql_content,
      "is_backup" => backup
    }
    form_data = if demand_url != "", do: Map.put(form_data, "demand_url", demand_url), else: form_data

    headers = %{
      "Content-Type" => "application/x-www-form-urlencoded; charset=UTF-8",
      "X-Requested-With" => "XMLHttpRequest",
      "X-CSRFToken" => get_csrf_token(client),
      "Referer" => "#{client.base_url}/submitsql/"
    }

    case http_post_form("#{client.base_url}/submitsql/", form_data, headers, client.cookie_jar) do
      {:ok, %{status: 200, body: body, redirect_url: redirect_url}} ->
        case Jason.decode(body) do
          {:ok, result} ->
            status = result["status"]
            if status == 0 do
              {:ok, result, client}
            else
              {:error, "Form submission error: #{inspect(result)}", client}
            end
          {:error, _} ->
            # Check for HTML success indicators
            if String.contains?(redirect_url, "sqlworkflow") or String.contains?(redirect_url, "/detail/") do
              {:ok, %{"status" => 0, "msg" => "Workflow submitted successfully"}, client}
            else
              # Try to extract error from HTML
              case Regex.run(~r/<div[^>]*class="[^"]*alert[^"]*"[^>]*>(.*?)<\/div>/s, body) do
                [_, alert_html] ->
                  error_msg = Regex.replace(~r/<[^>]+>/, alert_html, "") |> String.trim()
                  {:error, "Form submission error: #{error_msg}", client}
                _ ->
                  {:error, "Form submission failed: unexpected HTML response", client}
              end
            end
        end

      {:ok, %{status: status}} ->
        {:error, "Workflow submission failed: HTTP #{status}", client}

      {:error, reason} ->
        {:error, "Form submission failed: #{inspect(reason)}", client}
    end
  end

  # Helper functions

  defp ensure_authenticated(%__MODULE__{authenticated: true} = client) do
    {:ok, client}
  end
  defp ensure_authenticated(%__MODULE__{} = client) do
    login(client)
  end

  defp api_request(%__MODULE__{} = client, method, endpoint, data, params) do
    url = "#{client.base_url}#{endpoint}"
    url = if map_size(params) > 0 do
      query = URI.encode_query(params)
      "#{url}?#{query}"
    else
      url
    end

    csrf_token = get_csrf_token(client)

    headers = %{
      "X-CSRFToken" => csrf_token,
      "Referer" => client.base_url
    }
    headers = if method != :get, do: Map.put(headers, "Content-Type", "application/json"), else: headers

    body = if data, do: Jason.encode!(data), else: ""

    result = case method do
      :get -> http_get(url, headers, client.cookie_jar)
      :post -> http_post(url, body, headers, client.cookie_jar)
    end

    # Update client with new cookies from response
    updated_client = case result do
      {:ok, %{cookies: new_cookies}} ->
        %{client | cookie_jar: Map.merge(client.cookie_jar, new_cookies)}
      _ ->
        client
    end

    case result do
      {:ok, %{status: status, body: resp_body}} when status not in [200, 201] ->
        Logger.warning("[Archery] API #{method} #{url} -> status #{status}, body: #{String.slice(resp_body, 0, 300)}")
      {:ok, _} ->
        :ok
      {:error, reason} ->
        Logger.error("[Archery] API #{method} #{url} failed: #{inspect(reason)}")
    end

    case result do
      {:ok, %{status: status, body: body, cookies: _cookies}} ->
        {:ok, %{status: status, body: body}, updated_client}
      {:ok, %{status: status}} ->
        {:ok, %{status: status}, updated_client}
      {:error, reason} ->
        {:error, reason, updated_client}
    end
  end

  defp api_get(%__MODULE__{} = client, endpoint, params) do
    case api_request(client, :get, endpoint, nil, params) do
      {:ok, %{status: 200, body: body}, updated_client} ->
        case Jason.decode(body) do
          {:ok, result} -> {:ok, result, updated_client}
          {:error, _} -> {:error, "Failed to decode response", updated_client}
        end
      {:ok, %{status: status}, updated_client} ->
        {:error, "API request failed with status #{status}", updated_client}
      {:error, reason, updated_client} ->
        {:error, reason, updated_client}
    end
  end

  defp api_post(%__MODULE__{} = client, endpoint, data) do
    case api_request(client, :post, endpoint, data, %{}) do
      {:ok, %{status: 200, body: body}, updated_client} ->
        case Jason.decode(body) do
          {:ok, result} -> {:ok, result, updated_client}
          {:error, _} -> {:error, "Failed to decode response", updated_client}
        end
      {:ok, %{status: status}, updated_client} ->
        {:error, "API request failed with status #{status}", updated_client}
      {:error, reason, updated_client} ->
        {:error, "API request failed: #{inspect(reason)}", updated_client}
    end
  end

  defp http_get(url, headers, cookies) do
    request(:get, url, "", headers, cookies)
  end

  defp http_post(url, body, headers, cookies) do
    request(:post, url, body, headers, cookies)
  end

  defp http_post_form(url, form_data, headers, cookies) do
    body = URI.encode_query(form_data)
    request(:post, url, body, headers, cookies)
  end

  defp http_post_json(url, body, headers, cookies) do
    headers = Map.put(headers, "Content-Type", "application/json")
    request(:post, url, body, headers, cookies)
  end

  defp request(method, url, body, headers, cookies) do
    # Build cookie header
    cookie_header = if map_size(cookies) > 0 do
      cookie_str = Enum.map_join(cookies, "; ", fn {k, v} -> "#{k}=#{v}" end)
      Map.put(headers, "Cookie", cookie_str)
    else
      headers
    end

    # Convert headers to list format
    header_list = Enum.map(cookie_header, fn {k, v} -> {k, v} end)

    proxy_opts = case System.get_env("HTTPS_PROXY") || System.get_env("HTTP_PROXY") do
      nil -> []
      proxy -> [proxy: proxy]
    end

    options = [
      recv_timeout: 30_000,
      follow_redirect: true,
      max_redirect: 10
    ] ++ proxy_opts

    case HTTPoison.request(method, url, body, header_list, options) do
      {:ok, %HTTPoison.Response{status_code: status, body: resp_body, headers: resp_headers}} ->
        # Parse cookies from response
        new_cookies = parse_cookies(resp_headers, cookies)
        redirect_url = get_redirect_url(resp_headers)

        {:ok, %{
          status: status,
          body: resp_body,
          headers: resp_headers,
          cookies: new_cookies,
          redirect_url: redirect_url
        }}

      {:error, %HTTPoison.Error{reason: reason}} ->
        Logger.error("[Archery] HTTP request error: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp parse_cookies(headers, existing) do
    headers
    |> Enum.filter(fn {k, _} -> String.downcase(k) == "set-cookie" end)
    |> Enum.map(fn {_, v} ->
      # Parse cookie name and value
      case Regex.run(~r/^([^=]+)=([^;]+)/, v) do
        [_, name, value] -> {name, value}
        _ -> nil
      end
    end)
    |> Enum.reject(&is_nil/1)
    |> Enum.into(existing)
  end

  defp get_redirect_url(headers) do
    headers
    |> Enum.find(fn {k, _} -> String.downcase(k) == "location" end)
    |> case do
      {_, url} -> url
      _ -> ""
    end
  end

  defp extract_csrf_token(cookies, body) do
    # First try from cookies
    case Map.get(cookies, "csrftoken") do
      nil ->
        # Try to extract from HTML
        case Regex.run(~r/csrfmiddlewaretoken.*?value=["']([^"']+)["']/, body) do
          [_, token] -> token
          _ -> ""
        end
      token -> token
    end
  end

  defp get_csrf_token(%__MODULE__{cookie_jar: cookies}) do
    Map.get(cookies, "csrftoken", "")
  end

  defp extract_list(data, keys) when is_map(data) do
    Enum.find_value(keys, [], fn key ->
      case Map.get(data, key) do
        list when is_list(list) -> list
        _ -> nil
      end
    end)
  end
  defp extract_list(_, _), do: []

  defp get_config(key, default) do
    Application.get_env(:exhub, key, default)
  end

  defp return(value), do: value
end
