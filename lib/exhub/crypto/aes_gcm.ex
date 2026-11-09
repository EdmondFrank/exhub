defmodule Exhub.Crypto.AesGcm do
  @moduledoc """
  AES-256-GCM encryption helpers used by the Emacs password vault
  (`Exhub.ResponseHandlers.ExhubVault`) and by anything that stores secrets at
  rest (e.g. `Exhub.BlinkSearch.Backends.KeyValueStore`).

  The key is the SHA-256 digest of the master password from:

      config :exhub, :secret_vault, default: [password: System.get_env("SECRET_VAULT_PASSWORD", "")]

  ## Payload format

  `Base64(iv <> tag <> ciphertext)` where `iv` is a fresh 12-byte random nonce
  and `tag` is the 16-byte GCM authentication tag. Decryption fails on a wrong
  password or on a tampered payload.
  """

  @iv_length 12
  @tag_length 16
  @aad ""

  @type error :: {:error, String.t()}

  @doc "Returns true when a master password is configured."
  @spec configured?() :: boolean()
  def configured?, do: master_password() != ""

  @doc "Encrypts `plaintext` with the configured master password."
  @spec encrypt(binary()) :: {:ok, binary()} | error()
  def encrypt(plaintext), do: encrypt(master_password(), plaintext)

  @doc "Decrypts `ciphertext` with the configured master password."
  @spec decrypt(binary()) :: {:ok, binary()} | error()
  def decrypt(ciphertext), do: decrypt(master_password(), ciphertext)

  @doc "Encrypts `plaintext` with an explicitly supplied password."
  @spec encrypt(binary(), binary()) :: {:ok, binary()} | error()
  def encrypt("", _plaintext), do: {:error, "master password is not configured"}

  def encrypt(password, plaintext) when is_binary(password) and is_binary(plaintext) do
    try do
      key = derive_key(password)
      iv = :crypto.strong_rand_bytes(@iv_length)

      {ciphertext, tag} =
        :crypto.crypto_one_time_aead(:aes_256_gcm, key, iv, plaintext, @aad, true)

      {:ok, Base.encode64(iv <> tag <> ciphertext)}
    rescue
      e -> {:error, Exception.message(e)}
    end
  end

  def encrypt(_password, _plaintext), do: {:error, "value is not a binary"}

  @doc "Decrypts `ciphertext` with an explicitly supplied password."
  @spec decrypt(binary(), binary()) :: {:ok, binary()} | error()
  def decrypt("", _ciphertext), do: {:error, "master password is not configured"}

  def decrypt(password, ciphertext) when is_binary(password) and is_binary(ciphertext) do
    try do
      key = derive_key(password)

      case Base.decode64(ciphertext) do
        {:ok, decoded} -> open_payload(key, decoded)
        :error -> {:error, "invalid base64"}
      end
    rescue
      e -> {:error, Exception.message(e)}
    end
  end

  def decrypt(_password, _ciphertext), do: {:error, "ciphertext is not a binary"}

  @doc "Derives the 256-bit AES key from a master password."
  @spec derive_key(binary()) :: binary()
  def derive_key(password) when is_binary(password), do: :crypto.hash(:sha256, password)

  defp open_payload(key, decoded) do
    if byte_size(decoded) < @iv_length + @tag_length do
      {:error, "ciphertext is too short"}
    else
      iv = binary_part(decoded, 0, @iv_length)
      tag = binary_part(decoded, @iv_length, @tag_length)

      ciphertext =
        binary_part(
          decoded,
          @iv_length + @tag_length,
          byte_size(decoded) - @iv_length - @tag_length
        )

      case :crypto.crypto_one_time_aead(:aes_256_gcm, key, iv, ciphertext, @aad, tag, false) do
        :error -> {:error, "decryption failed (wrong password or tampered data)"}
        plaintext -> {:ok, plaintext}
      end
    end
  end

  defp master_password do
    case Application.get_env(:exhub, :secret_vault) do
      config when is_list(config) -> config |> Keyword.get(:default, []) |> password_from()
      _ -> ""
    end
  end

  defp password_from(default) when is_list(default), do: Keyword.get(default, :password) || ""
  defp password_from(_default), do: ""
end
