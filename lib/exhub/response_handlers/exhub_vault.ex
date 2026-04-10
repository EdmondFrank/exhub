defmodule Exhub.ResponseHandlers.ExhubVault do
  @moduledoc """
  Vault encrypt/decrypt handler for Emacs org-mode password vault.

  Handles AES-256-GCM encryption and decryption of secrets, using a master
  password derived from application config. Results are sent back to Emacs
  via the Exhub message bus.
  """

  require Logger

  @iv_length 12
  @tag_length 16

  def call(["exhub-vault", "encrypt", callback, plaintext]) do
    password = get_master_password()

    case encrypt(password, plaintext) do
      {:ok, encoded} ->
        Exhub.send_message(~s[(#{callback} "#{encoded}")])

      {:error, reason} ->
        Exhub.send_message(~s[(message "Vault encrypt error: #{reason}")])
    end
  end

  def call(["exhub-vault", "decrypt", callback, ciphertext]) do
    password = get_master_password()

    case decrypt(password, ciphertext) do
      {:ok, plaintext} ->
        escaped = escape_for_elisp(plaintext)
        Exhub.send_message(~s[(#{callback} "#{escaped}")])

      {:error, reason} ->
        Exhub.send_message(~s[(message "Vault decrypt error: #{reason}")])
    end
  end

  defp get_master_password do
    Application.get_env(:exhub, :secret_vault)[:default][:password] || ""
  end

  defp derive_key(password) do
    :crypto.hash(:sha256, password)
  end

  defp encrypt("", _plaintext), do: {:error, "master password is not configured"}

  defp encrypt(password, plaintext) do
    try do
      key = derive_key(password)
      iv = :crypto.strong_rand_bytes(@iv_length)
      {ciphertext, tag} =
        :crypto.crypto_one_time_aead(:aes_256_gcm, key, iv, plaintext, "", true)

      {:ok, Base.encode64(iv <> tag <> ciphertext)}
    rescue
      e -> {:error, Exception.message(e)}
    end
  end

  defp decrypt("", _ciphertext), do: {:error, "master password is not configured"}

  defp decrypt(password, ciphertext) do
    try do
      key = derive_key(password)

      case Base.decode64(ciphertext) do
        {:ok, decoded} ->
          if byte_size(decoded) < @iv_length + @tag_length do
            {:error, "ciphertext is too short"}
          else
            iv = binary_part(decoded, 0, @iv_length)
            tag = binary_part(decoded, @iv_length, @tag_length)
            ct = binary_part(decoded, @iv_length + @tag_length, byte_size(decoded) - @iv_length - @tag_length)

            case :crypto.crypto_one_time_aead(:aes_256_gcm, key, iv, ct, "", tag, false) do
              :error -> {:error, "decryption failed (wrong password or tampered data)"}
              plaintext -> {:ok, plaintext}
            end
          end

        :error ->
          {:error, "invalid base64"}
      end
    rescue
      e -> {:error, Exception.message(e)}
    end
  end

  defp escape_for_elisp(plaintext) do
    plaintext
    |> String.replace("\\", "\\\\")
    |> String.replace("\"", "\\\"")
    |> String.replace("\n", "\\n")
  end
end
