defmodule Exhub.MCP.Hub.EnvTemplateTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  alias Exhub.MCP.Hub.EnvTemplate

  # A name unlikely to collide with the ambient environment.
  @set_var "EXHUB_ENV_TEMPLATE_TEST_SET"

  setup do
    System.delete_env(@set_var)
    on_exit(fn -> System.delete_env(@set_var) end)
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

    test "leaves the placeholder literal and warns when the variable is unset" do
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
end
