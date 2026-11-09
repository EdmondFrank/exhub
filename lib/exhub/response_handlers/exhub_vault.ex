defmodule Exhub.ResponseHandlers.ExhubVault do
  @moduledoc """
  Vault encrypt/decrypt handler for Emacs org-mode password vault.

  Handles AES-256-GCM encryption and decryption of secrets, using a master
  password derived from application config. Results are sent back to Emacs
  via the Exhub message bus.

  The crypto primitives live in `Exhub.Crypto.AesGcm`, shared with other
  components that need encryption at rest (e.g. the blink-search key-value
  store).
  """

  alias Exhub.Crypto.AesGcm

  def call(["exhub-vault", "encrypt", callback, plaintext]) do
    case AesGcm.encrypt(plaintext) do
      {:ok, encoded} ->
        Exhub.send_message(~s[(#{callback} "#{encoded}")])

      {:error, reason} ->
        Exhub.send_message(~s[(message "Vault encrypt error: #{reason}")])
    end
  end

  def call(["exhub-vault", "decrypt", callback, ciphertext]) do
    case AesGcm.decrypt(ciphertext) do
      {:ok, plaintext} ->
        escaped = escape_for_elisp(plaintext)
        Exhub.send_message(~s[(#{callback} "#{escaped}")])

      {:error, reason} ->
        Exhub.send_message(~s[(message "Vault decrypt error: #{reason}")])
    end
  end

  defp escape_for_elisp(plaintext) do
    plaintext
    |> String.replace("\\", "\\\\")
    |> String.replace("\"", "\\\"")
    |> String.replace("\n", "\\n")
  end
end
