defmodule Exhub.BrowserAgent.KuriTest do
  use ExUnit.Case, async: false

  alias Exhub.BrowserAgent.{Kuri, KuriCli, KuriHttp}

  describe "backend/0" do
    test "reads the backend from the feature namespace" do
      previous = Application.get_env(:exhub, Exhub.BrowserAgent)

      on_exit(fn ->
        if previous do
          Application.put_env(:exhub, Exhub.BrowserAgent, previous)
        else
          Application.delete_env(:exhub, Exhub.BrowserAgent)
        end
      end)

      Application.put_env(:exhub, Exhub.BrowserAgent, backend: :cli)
      assert Kuri.backend() == KuriCli

      Application.put_env(:exhub, Exhub.BrowserAgent, backend: :http)
      assert Kuri.backend() == KuriHttp
    end
  end

  describe "backend_for/1" do
    test "defaults to the daemon HTTP backend" do
      assert Kuri.backend_for(nil) == KuriHttp
      assert Kuri.backend_for(:http) == KuriHttp
    end

    test "selects the CLI backend when configured" do
      assert Kuri.backend_for(:cli) == KuriCli
    end

    test "accepts an explicit backend module" do
      assert Kuri.backend_for(Exhub.BrowserAgent.KuriTest.Stub) ==
               Exhub.BrowserAgent.KuriTest.Stub
    end
  end
end
