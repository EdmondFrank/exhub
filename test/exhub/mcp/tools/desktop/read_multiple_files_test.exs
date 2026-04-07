defmodule Exhub.MCP.Tools.Desktop.ReadMultipleFilesTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Desktop.ReadMultipleFiles

  describe "execute/2" do
    setup do
      tmp_dir = Path.join(System.tmp_dir!(), "read_multiple_files_test_#{:rand.uniform(999_999)}")
      File.mkdir_p!(tmp_dir)
      on_exit(fn -> File.rm_rf!(tmp_dir) end)
      {:ok, tmp_dir: tmp_dir}
    end

    test "reads multiple files successfully", %{tmp_dir: tmp_dir} do
      file1 = Path.join(tmp_dir, "file1.txt")
      file2 = Path.join(tmp_dir, "file2.txt")
      file3 = Path.join(tmp_dir, "file3.txt")

      File.write!(file1, "Content of file 1")
      File.write!(file2, "Content of file 2")
      File.write!(file3, "Content of file 3")

      frame = %{}
      {:reply, resp, _frame} = ReadMultipleFiles.execute(%{paths: [file1, file2, file3]}, frame)

      assert resp.isError == false
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      {:ok, decoded} = Toon.decode(text)
      results = decoded["results"]

      assert length(results) == 3

      assert Enum.at(results, 0)["success"] == true
      assert Enum.at(results, 0)["content"] == "Content of file 1"

      assert Enum.at(results, 1)["success"] == true
      assert Enum.at(results, 1)["content"] == "Content of file 2"

      assert Enum.at(results, 2)["success"] == true
      assert Enum.at(results, 2)["content"] == "Content of file 3"
    end

    test "handles mixed success and failure", %{tmp_dir: tmp_dir} do
      existing_file = Path.join(tmp_dir, "exists.txt")
      missing_file = Path.join(tmp_dir, "missing.txt")

      File.write!(existing_file, "I exist")

      frame = %{}
      {:reply, resp, _frame} = ReadMultipleFiles.execute(%{paths: [existing_file, missing_file]}, frame)

      assert resp.isError == false
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      {:ok, decoded} = Toon.decode(text)
      results = decoded["results"]

      assert length(results) == 2

      # First file should succeed
      assert Enum.at(results, 0)["success"] == true
      assert Enum.at(results, 0)["content"] == "I exist"

      # Second file should fail
      assert Enum.at(results, 1)["success"] == false
      assert Enum.at(results, 1)["error"] =~ "File not found"
    end

    test "offset parameter works correctly", %{tmp_dir: tmp_dir} do
      file = Path.join(tmp_dir, "offset_test.txt")

      lines = for i <- 1..10, do: "Line #{i}"
      File.write!(file, Enum.join(lines, "\n"))

      frame = %{}
      {:reply, resp, _frame} = ReadMultipleFiles.execute(%{paths: [file], offset: 5}, frame)

      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      {:ok, decoded} = Toon.decode(text)
      result = hd(decoded["results"])

      assert result["success"] == true
      assert result["content"] == "Line 6\nLine 7\nLine 8\nLine 9\nLine 10"
      assert result["lines_read"] == 5
      assert result["total_lines"] == 10
    end

    test "length parameter works correctly", %{tmp_dir: tmp_dir} do
      file = Path.join(tmp_dir, "length_test.txt")

      lines = for i <- 1..100, do: "Line #{i}"
      File.write!(file, Enum.join(lines, "\n"))

      frame = %{}
      {:reply, resp, _frame} = ReadMultipleFiles.execute(%{paths: [file], length: 10}, frame)

      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      {:ok, decoded} = Toon.decode(text)
      result = hd(decoded["results"])

      assert result["success"] == true
      assert result["lines_read"] == 10
      assert result["total_lines"] == 100

      content_lines = result["content"] |> String.split("\n")
      assert length(content_lines) == 10
    end

    test "results maintain order matching input paths", %{tmp_dir: tmp_dir} do
      file_a = Path.join(tmp_dir, "a.txt")
      file_b = Path.join(tmp_dir, "b.txt")
      file_c = Path.join(tmp_dir, "c.txt")

      File.write!(file_a, "A content")
      File.write!(file_b, "B content")
      File.write!(file_c, "C content")

      # Pass in non-alphabetical order
      frame = %{}
      {:reply, resp, _frame} = ReadMultipleFiles.execute(%{paths: [file_c, file_a, file_b]}, frame)

      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      {:ok, decoded} = Toon.decode(text)
      results = decoded["results"]

      # Results should maintain the input order
      assert Enum.at(results, 0)["content"] == "C content"
      assert Enum.at(results, 1)["content"] == "A content"
      assert Enum.at(results, 2)["content"] == "B content"
    end

    test "handles empty paths list" do
      frame = %{}
      {:reply, resp, _frame} = ReadMultipleFiles.execute(%{paths: []}, frame)

      assert resp.isError == false
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      {:ok, decoded} = Toon.decode(text)
      results = decoded["results"]

      assert results == []
    end

    test "parallel execution reads multiple files concurrently", %{tmp_dir: tmp_dir} do
      # Create many files to verify parallel execution
      files =
        for i <- 1..20 do
          path = Path.join(tmp_dir, "parallel_#{i}.txt")
          File.write!(path, "Content #{i}")
          path
        end

      frame = %{}
      {:reply, resp, _frame} = ReadMultipleFiles.execute(%{paths: files}, frame)

      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      {:ok, decoded} = Toon.decode(text)
      results = decoded["results"]

      assert length(results) == 20

      # All files should be read successfully
      for result <- results do
        assert result["success"] == true
      end

      # Verify content matches expected pattern
      contents = Enum.map(results, & &1["content"])
      for i <- 1..20 do
        assert "Content #{i}" in contents
      end
    end
  end
end
