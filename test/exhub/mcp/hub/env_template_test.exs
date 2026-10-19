defmodule Exhub.MCP.Hub.EnvTemplateTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  alias Exhub.MCP.Hub.EnvTemplate

  # A name unlikely to collide with the ambient environment.
  @set_var "EXHUB_ENV_TEMPLATE_TEST_SET"

  setup do
    # Keep the pure expand tests off the real envless vault unless a test opts
    # in explicitly.
    Application.put_env(:exhub, EnvTemplate, envless: false)
    System.delete_env(@set_var)

    on_exit(fn ->
      System.delete_env(@set_var)
      Application.delete_env(:exhub, EnvTemplate)
    end)

    :ok
  end

  describe "expand_string/1" do
    test "substitutes a set variable" do
      System.put_env(@set_var, "s3cret")
      assert EnvTemplate.expand_string("Bearer ${#{@set_var}}") == "Bearer s3cret"
    end

    test "substitutes every placeholder in the string" do
      System.put_env(@set_var, "v")
      assert EnvTemplate.expand_string("${#{@set_var}}-${#{@set_var}}") == "v-v"
    end

    test "leaves the placeholder literal and warns when the variable is unresolved" do
      log =
        capture_log(fn ->
          assert EnvTemplate.expand_string("${#{@set_var}}") == "${#{@set_var}}"
        end)

      assert log =~ @set_var
    end

    test "passes through strings without placeholders" do
      assert EnvTemplate.expand_string("plain value") == "plain value"
    end

    test "ignores malformed placeholders" do
      assert EnvTemplate.expand_string("${1BAD}") == "${1BAD}"
      assert EnvTemplate.expand_string("$ {NOT_A_TEMPLATE}") == "$ {NOT_A_TEMPLATE}"
    end
  end

  describe "expand_value/1" do
    test "walks lists and map values" do
      System.put_env(@set_var, "tok")

      assert EnvTemplate.expand_value(["a", "${#{@set_var}}"]) == ["a", "tok"]
      assert EnvTemplate.expand_value(%{"K" => "${#{@set_var}}"}) == %{"K" => "tok"}
    end

    test "returns non-string values unchanged" do
      assert EnvTemplate.expand_value(nil) == nil
      assert EnvTemplate.expand_value(:atom) == :atom
      assert EnvTemplate.expand_value(42) == 42
    end
  end

  describe "envless vault fallback" do
    setup do
      path = fake_envless(~s(printf %s "vault-val"\n))
      on_exit(fn -> File.rm(path) end)
      Application.put_env(:exhub, EnvTemplate, envless: true, envless_path: path)
      :ok
    end

    test "reads a value from the vault when it is not in the environment" do
      System.delete_env(@set_var)
      assert EnvTemplate.expand_string("Bearer ${#{@set_var}}") == "Bearer vault-val"
    end

    test "the environment takes precedence over the vault" do
      System.put_env(@set_var, "from-env")
      assert EnvTemplate.expand_string("${#{@set_var}}") == "from-env"
    end
  end

  describe "vault miss handling" do
    test "leaves the placeholder literal and warns when the lookup fails" do
      path = fake_envless("exit 1\n")
      on_exit(fn -> File.rm(path) end)
      Application.put_env(:exhub, EnvTemplate, envless: true, envless_path: path)
      System.delete_env(@set_var)

      log =
        capture_log(fn ->
          assert EnvTemplate.expand_string("${#{@set_var}}") == "${#{@set_var}}"
        end)

      assert log =~ @set_var
    end

    test "leaves the placeholder literal when the vault fallback is disabled" do
      Application.put_env(:exhub, EnvTemplate, envless: false)
      System.delete_env(@set_var)

      log =
        capture_log(fn ->
          assert EnvTemplate.expand_string("${#{@set_var}}") == "${#{@set_var}}"
        end)

      assert log =~ @set_var
    end
  end

  defp fake_envless(body) do
    path =
      Path.join(
        System.tmp_dir!(),
        "exhub_fake_envless_#{System.unique_integer([:positive])}"
      )

    File.write!(path, "#!/bin/sh\n" <> body)
    File.chmod!(path, 0o755)
    path
  end
end
