defmodule Exhub.Memory.SecretScan do
  @moduledoc """
  Pre-write secret detection for memory notes.

  Memory is shared with every future agent and may be promoted into a committed
  skill file, so nothing that looks like a credential may be written. This is a
  best-effort guard (Beacon's "never put secrets in memory" boundary), not a
  full DLP engine.
  """

  @findings [
    {~r/-----BEGIN [A-Z ]*PRIVATE KEY-----/, "private key block"},
    {~r/\bAKIA[0-9A-Z]{16}\b/, "aws access key id"},
    {~r/\bghp_[A-Za-z0-9]{20,}\b/, "github token"},
    {~r/\bgithub_pat_[A-Za-z0-9_]{20,}\b/, "github fine-grained token"},
    {~r/\bsk-[A-Za-z0-9]{16,}\b/, "api key"},
    {~r/\bxox[baprs]-[A-Za-z0-9-]{10,}\b/, "slack token"},
    {~r/\beyJ[A-Za-z0-9_-]{10,}\.[A-Za-z0-9_-]{10,}\.[A-Za-z0-9_-]{10,}\b/, "jwt"},
    {~r/\bBearer\s+[A-Za-z0-9._-]{20,}/i, "bearer token"},
    {~r/(?i)\b(?:password|passwd|secret|api[_-]?key|access[_-]?token)\s*[:=]\s*\S{8,}/,
     "inline credential"},
    {~r/\b\d{3}-\d{2}-\d{4}\b/, "possible ssn"}
  ]

  @doc "Return `:ok` when no secret pattern matches, or `{:error, [labels]}`."
  @spec scan(term()) :: :ok | {:error, [String.t()]}
  def scan(value) do
    case scan_many([value]) do
      :ok -> :ok
      {:error, findings} -> {:error, findings}
    end
  end

  @doc "Scan several fields (title, body, tags, …) and merge the findings."
  @spec scan_many([term()]) :: :ok | {:error, [String.t()]}
  def scan_many(values) do
    findings =
      values
      |> Enum.filter(&is_binary/1)
      |> Enum.flat_map(fn text ->
        for {re, label} <- @findings, Regex.match?(re, text), do: label
      end)
      |> Enum.uniq()

    case findings do
      [] -> :ok
      labels -> {:error, labels}
    end
  end

  @doc "Replace every matched secret with `[REDACTED]`."
  @spec redact(String.t()) :: String.t()
  def redact(text) when is_binary(text) do
    Enum.reduce(@findings, text, fn {re, _label}, acc ->
      Regex.replace(re, acc, "[REDACTED]")
    end)
  end

  def redact(value), do: value
end
