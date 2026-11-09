defmodule Exhub.Crypto.AesGcmTest do
  use ExUnit.Case, async: false

  alias Exhub.Crypto.AesGcm

  # The master password comes from global application config, so these tests
  # must not run concurrently with other config-mutating suites.
  setup do
    previous = Application.get_env(:exhub, :secret_vault)
    Application.put_env(:exhub, :secret_vault, default: [password: "test-master-password"])
    on_exit(fn -> restore(previous) end)
    :ok
  end

  defp restore(nil), do: Application.delete_env(:exhub, :secret_vault)
  defp restore(config), do: Application.put_env(:exhub, :secret_vault, config)

  describe "configured?/0" do
    test "is true when a master password is set" do
      assert AesGcm.configured?()
    end

    test "is false when no master password is set" do
      Application.delete_env(:exhub, :secret_vault)

      refute AesGcm.configured?()
    end
  end

  describe "encrypt/1 and decrypt/1" do
    test "round-trips plaintext" do
      assert {:ok, encrypted} = AesGcm.encrypt("s3cr3t value")
      assert encrypted != "s3cr3t value"
      assert {:ok, "s3cr3t value"} = AesGcm.decrypt(encrypted)
    end

    test "round-trips an empty string" do
      assert {:ok, encrypted} = AesGcm.encrypt("")
      assert {:ok, ""} = AesGcm.decrypt(encrypted)
    end

    test "round-trips unicode and newlines" do
      plaintext = "pässwörd\nwith newline — 密码"
      assert {:ok, encrypted} = AesGcm.encrypt(plaintext)
      assert {:ok, ^plaintext} = AesGcm.decrypt(encrypted)
    end

    test "uses a fresh IV for every encryption" do
      assert {:ok, first} = AesGcm.encrypt("same")
      assert {:ok, second} = AesGcm.encrypt("same")

      assert first != second
      assert {:ok, "same"} = AesGcm.decrypt(first)
      assert {:ok, "same"} = AesGcm.decrypt(second)
    end

    test "reports a missing master password" do
      Application.delete_env(:exhub, :secret_vault)

      assert {:error, "master password is not configured"} = AesGcm.encrypt("value")
      assert {:error, "master password is not configured"} = AesGcm.decrypt("anything")
    end

    test "rejects non-binary input" do
      assert {:error, "value is not a binary"} = AesGcm.encrypt(nil)
      assert {:error, "ciphertext is not a binary"} = AesGcm.decrypt(nil)
    end
  end

  describe "decrypt/1 failure modes" do
    test "fails with the wrong password" do
      assert {:ok, encrypted} = AesGcm.encrypt("s3cr3t")

      assert {:error, "decryption failed (wrong password or tampered data)"} =
               AesGcm.decrypt("wrong-password", encrypted)
    end

    test "detects a tampered payload" do
      assert {:ok, encrypted} = AesGcm.encrypt("s3cr3t")
      assert {:ok, raw} = Base.decode64(encrypted)

      size = byte_size(raw)
      <<prefix::binary-size(size - 1), last::8>> = raw
      tampered = Base.encode64(prefix <> <<Bitwise.bxor(last, 1)>>)

      assert {:error, "decryption failed (wrong password or tampered data)"} =
               AesGcm.decrypt(tampered)
    end

    test "rejects invalid base64" do
      assert {:error, "invalid base64"} = AesGcm.decrypt("not base64!!")
    end

    test "rejects a payload shorter than iv + tag" do
      assert {:error, "ciphertext is too short"} =
               AesGcm.decrypt(Base.encode64(:binary.copy(<<0>>, 20)))
    end
  end

  describe "derive_key/1" do
    test "returns a 32-byte SHA-256 digest" do
      key = AesGcm.derive_key("password")

      assert byte_size(key) == 32
      assert key == :crypto.hash(:sha256, "password")
      assert key != AesGcm.derive_key("Password")
    end
  end
end
