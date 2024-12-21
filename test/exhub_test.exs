defmodule ExhubTest do
  use ExUnit.Case
  doctest Exhub

  test "greets the world" do
    assert Exhub.hello() == :world
  end
end
