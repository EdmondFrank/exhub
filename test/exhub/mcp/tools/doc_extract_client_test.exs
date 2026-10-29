defmodule Exhub.MCP.Tools.DocExtract.ClientTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.DocExtract
  alias Exhub.MCP.Tools.DocExtract.Client

  describe "default_model/0" do
    test "is MoArk's latest OCR model" do
      assert Client.default_model() == "Unlimited-OCR-2"
    end
  end

  describe "doc_extract tool description" do
    test "advertises Unlimited-OCR-2 as the default" do
      description = DocExtract.description()

      assert description =~ "`Unlimited-OCR-2` (default)"
    end

    test "keeps the older models selectable" do
      description = DocExtract.description()

      assert description =~ "`Unlimited-OCR` — Previous generation"
      assert description =~ "MinerU2.5-Pro"
    end
  end
end
