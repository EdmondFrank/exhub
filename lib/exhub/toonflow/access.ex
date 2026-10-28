defmodule Exhub.Toonflow.Access do
  @moduledoc """
  Address gate for Toonflow's mutating endpoints (create / run / edit / render /
  assist).

  `ui.require_local` (default `true`) keeps those endpoints loopback-only because
  the app is also reachable over the LAN and VPN. Three cases are still "local":

    * loopback — `127.0.0.0/8`, `::1`, and their IPv4-mapped IPv6 forms;
    * the host's own interface addresses — a browser on this machine may reach
      the canvas through `192.168.x.x` or a VPN address instead of `localhost`,
      and the kernel uses that address as the source too;
    * networks listed in `ui.allowed_networks` — IPv4 **and** IPv6 CIDRs (or bare
      addresses) for trusted remote devices, e.g.
      `["10.7.0.0/24", "fd00::/8"]`.

  `writable?/1` is the only side-effectful entry point (it reads config and the
  host interfaces); the matching helpers are pure and unit-tested.
  """

  import Bitwise

  alias Exhub.Toonflow.Config

  @doc "Whether `ip` may call a mutating Toonflow endpoint. Reads `toonflow.ui`."
  @spec writable?(term()) :: boolean()
  def writable?(ip) do
    ip = normalize(ip)

    case Config.get("ui", %{}) do
      %{"require_local" => false} ->
        true

      %{} = ui ->
        loopback?(ip) or host_ip?(ip) or allowed_network?(ip, ui["allowed_networks"])

      _ ->
        loopback?(ip) or host_ip?(ip)
    end
  end

  @doc "The peer address as a printable string, for logs and error bodies."
  @spec format(term()) :: String.t()
  def format(ip) do
    case normalize(ip) do
      {_, _, _, _} = tuple -> to_string(:inet.ntoa(tuple))
      {_, _, _, _, _, _, _, _} = tuple -> to_string(:inet.ntoa(tuple))
      other -> inspect(other)
    end
  end

  @doc "Rewrites an IPv4-mapped IPv6 address (`::ffff:a.b.c.d`) to plain IPv4."
  @spec normalize(term()) :: term()
  def normalize({0, 0, 0, 0, 0, 0xFFFF, hi, lo}) do
    {hi >>> 8 &&& 0xFF, hi &&& 0xFF, lo >>> 8 &&& 0xFF, lo &&& 0xFF}
  end

  def normalize(ip), do: ip

  @doc "Loopback IPv4 (`127.0.0.0/8`) or IPv6 (`::1`). Pure."
  @spec loopback?(term()) :: boolean()
  def loopback?({127, _, _, _}), do: true
  def loopback?({0, 0, 0, 0, 0, 0, 0, 1}), do: true
  def loopback?(_ip), do: false

  @doc "True when `ip` is one of this host's interface addresses."
  @spec host_ip?(term()) :: boolean()
  def host_ip?(ip) do
    ip = normalize(ip)

    case :inet.getifaddrs() do
      {:ok, interfaces} ->
        Enum.any?(interfaces, fn {_name, opts} ->
          Enum.any?(opts, fn
            {:addr, addr} -> normalize(addr) == ip
            _ -> false
          end)
        end)

      _ ->
        false
    end
  end

  @doc "True when `ip` matches an entry of `networks` (IPv4/IPv6 CIDR or address)."
  @spec allowed_network?(term(), term()) :: boolean()
  def allowed_network?(_ip, networks) when networks in [nil, []], do: false

  def allowed_network?(ip, networks) when is_list(networks) do
    Enum.any?(networks, &in_network?(ip, &1))
  end

  def allowed_network?(_ip, _networks), do: false

  @doc """
  True when `ip` falls inside `network` (`"10.7.0.0/24"`, `"10.7.0.14"`,
  `"fd00::/8"`, `"::1"`). The address families must match; invalid entries never
  match. Pure.
  """
  @spec in_network?(term(), term()) :: boolean()
  def in_network?(ip, network) when is_binary(network) do
    case normalize(ip) do
      {_, _, _, _} = v4 -> in_cidr?(v4, network)
      {_, _, _, _, _, _, _, _} = v6 -> in_cidr?(v6, network)
      _ -> false
    end
  end

  def in_network?(_ip, _network), do: false

  defp in_cidr?(ip, network) do
    with {:ok, base, prefix} <- parse_network(network),
         true <- tuple_size(base) == tuple_size(ip),
         width = bit_width(ip),
         true <- prefix >= 0 and prefix <= width do
      mask = prefix_mask(prefix, width)
      (ip_int(ip) &&& mask) == (ip_int(base) &&& mask)
    else
      _ -> false
    end
  end

  defp parse_network(network) do
    case String.split(String.trim(network), "/", parts: 2) do
      [address] ->
        with {:ok, ip} <- parse_ip(address), do: {:ok, ip, bit_width(ip)}

      [address, prefix] ->
        with {:ok, ip} <- parse_ip(address),
             {bits, ""} <- Integer.parse(String.trim(prefix)) do
          {:ok, ip, bits}
        else
          _ -> :error
        end
    end
  end

  defp parse_ip(address) do
    charlist = String.to_charlist(address)

    case :inet.parse_ipv4strict_address(charlist) do
      {:ok, ip} ->
        {:ok, ip}

      _ ->
        case :inet.parse_ipv6strict_address(charlist) do
          {:ok, ip} -> {:ok, ip}
          _ -> :error
        end
    end
  end

  defp ip_int({a, b, c, d}), do: a <<< 24 ||| b <<< 16 ||| c <<< 8 ||| d

  defp ip_int(tuple) when tuple_size(tuple) == 8 do
    tuple
    |> Tuple.to_list()
    |> Enum.reduce(0, fn part, acc -> acc <<< 16 ||| part end)
  end

  defp bit_width(ip), do: if(tuple_size(ip) == 4, do: 32, else: 128)

  defp prefix_mask(prefix, _width) when prefix <= 0, do: 0

  defp prefix_mask(prefix, width) do
    ((1 <<< prefix) - 1) <<< (width - prefix) &&& (1 <<< width) - 1
  end
end
