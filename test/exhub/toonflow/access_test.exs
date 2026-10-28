defmodule Exhub.Toonflow.AccessTest do
  use ExUnit.Case, async: false

  alias Exhub.Toonflow.Access

  # TEST-NET-3: never a local interface address, so `host_ip?/1` cannot make the
  # "foreign peer" assertions pass by accident.
  @foreign {203, 0, 113, 7}

  setup do
    previous = Application.get_env(:exhub, :toonflow)

    on_exit(fn ->
      if previous,
        do: Application.put_env(:exhub, :toonflow, previous),
        else: Application.delete_env(:exhub, :toonflow)
    end)

    :ok
  end

  describe "writable?/1" do
    test "always allows loopback" do
      Application.put_env(:exhub, :toonflow, %{"ui" => %{"require_local" => true}})

      assert Access.writable?({127, 0, 0, 1})
      assert Access.writable?({127, 4, 5, 6})
      assert Access.writable?({0, 0, 0, 0, 0, 0, 0, 1})
    end

    test "rejects a foreign address when require_local is on" do
      Application.put_env(:exhub, :toonflow, %{"ui" => %{"require_local" => true}})
      refute Access.writable?(@foreign)
    end

    test "allows everyone when require_local is false" do
      Application.put_env(:exhub, :toonflow, %{"ui" => %{"require_local" => false}})
      assert Access.writable?(@foreign)
    end

    test "allows a LAN/VPN peer listed in allowed_networks" do
      Application.put_env(:exhub, :toonflow, %{
        "ui" => %{"require_local" => true, "allowed_networks" => ["10.7.0.0/24"]}
      })

      assert Access.writable?({10, 7, 0, 14})
      assert Access.writable?({10, 7, 0, 255})
      refute Access.writable?({10, 8, 0, 14})
    end

    test "treats a bare address as a /32" do
      Application.put_env(:exhub, :toonflow, %{
        "ui" => %{"require_local" => true, "allowed_networks" => ["10.7.0.14"]}
      })

      assert Access.writable?({10, 7, 0, 14})
      refute Access.writable?({10, 7, 0, 15})
    end

    test "unwraps an IPv4-mapped peer before matching" do
      Application.put_env(:exhub, :toonflow, %{
        "ui" => %{"require_local" => true, "allowed_networks" => ["10.7.0.0/24"]}
      })

      assert Access.writable?({0, 0, 0, 0, 0, 0xFFFF, 0x0A07, 0x000E})
      refute Access.writable?({0, 0, 0, 0, 0, 0xFFFF, 0x0A08, 0x0001})
    end

    test "the host's own interface addresses pass" do
      Application.put_env(:exhub, :toonflow, %{"ui" => %{"require_local" => true}})

      assert Enum.all?(local_addresses(), &Access.writable?/1)
    end
  end

  describe "loopback?/1" do
    test "matches 127/8 and ::1 only" do
      assert Access.loopback?({127, 0, 0, 1})
      assert Access.loopback?({0, 0, 0, 0, 0, 0, 0, 1})
      refute Access.loopback?({10, 7, 0, 21})
      refute Access.loopback?(nil)
    end
  end

  describe "normalize/1 + format/1" do
    test "unwraps IPv4-mapped IPv6 and prints addresses" do
      assert Access.normalize({0, 0, 0, 0, 0, 0xFFFF, 0x0A07, 0x000E}) == {10, 7, 0, 14}
      assert Access.normalize({10, 7, 0, 14}) == {10, 7, 0, 14}
      assert Access.normalize({0, 0, 0, 0, 0, 0, 0, 1}) == {0, 0, 0, 0, 0, 0, 0, 1}

      assert Access.format({10, 7, 0, 14}) == "10.7.0.14"
      assert Access.format({0, 0, 0, 0, 0, 0xFFFF, 0x0A07, 0x000E}) == "10.7.0.14"
      assert Access.format({0, 0, 0, 0, 0, 0, 0, 1}) == "::1"
      assert Access.format(nil) == "nil"
    end
  end

  describe "in_network?/2" do
    test "matches IPv4 CIDRs and bare addresses" do
      assert Access.in_network?({10, 7, 0, 14}, "10.7.0.0/24")
      assert Access.in_network?({192, 168, 1, 20}, "192.168.1.20")
      assert Access.in_network?({1, 2, 3, 4}, "0.0.0.0/0")
      refute Access.in_network?({192, 168, 2, 20}, "192.168.1.0/24")
    end

    test "matches IPv6 CIDRs and bare addresses" do
      assert Access.in_network?({0, 0, 0, 0, 0, 0, 0, 1}, "::1/128")
      assert Access.in_network?({0xFD00, 0, 0, 0, 0, 0, 0, 8}, "fd00::/8")
      assert Access.in_network?({0xFE80, 0, 0, 0, 1, 2, 3, 4}, "fe80::/10")
      refute Access.in_network?({0xFD00, 0, 0, 0, 0, 0, 0, 8}, "fd00:1::/32")
    end

    test "ignores invalid entries and mismatched families" do
      refute Access.in_network?({10, 7, 0, 14}, "nonsense")
      refute Access.in_network?({10, 7, 0, 14}, "10.7.0.0/33")
      refute Access.in_network?({10, 7, 0, 14}, "10.7.0.0/abc")
      refute Access.in_network?({0, 0, 0, 0, 0, 0, 0, 1}, "::1/129")
      refute Access.in_network?({0, 0, 0, 0, 0, 0, 0, 1}, "10.7.0.0/24")
      refute Access.in_network?({10, 7, 0, 14}, "::1/128")
      refute Access.in_network?(@foreign, nil)
      refute Access.in_network?(@foreign, "10.7.0.0/24")
    end

    test "trims surrounding whitespace in configured entries" do
      assert Access.in_network?({10, 7, 0, 14}, "  10.7.0.0/24  ")
    end
  end

  describe "allowed_network?/2" do
    test "is false for empty or malformed lists" do
      refute Access.allowed_network?(@foreign, [])
      refute Access.allowed_network?(@foreign, nil)
      refute Access.allowed_network?(@foreign, "10.7.0.0/24")

      assert Access.allowed_network?(@foreign, ["10.0.0.0/8", "203.0.113.0/24"])
    end
  end

  defp local_addresses do
    case :inet.getifaddrs() do
      {:ok, interfaces} -> for {_name, opts} <- interfaces, {:addr, addr} <- opts, do: addr
      _ -> []
    end
  end
end
