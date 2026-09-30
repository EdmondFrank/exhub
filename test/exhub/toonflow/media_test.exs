defmodule Exhub.Toonflow.MediaTest do
  use ExUnit.Case, async: false

  alias Exhub.Toonflow.{Media, Store}

  setup do
    root = Path.join(System.tmp_dir!(), "toonflow_media_#{System.unique_integer([:positive])}")
    server = :"toonflow_media_store_#{System.unique_integer([:positive])}"
    {:ok, pid} = Store.start_link(root_dir: root, name: server)

    previous = Application.get_env(:exhub, :toonflow_media_client)
    Application.put_env(:exhub, :toonflow_media_client, Exhub.Toonflow.MediaTest.StubClient)

    on_exit(fn ->
      if Process.alive?(pid), do: GenServer.stop(pid)
      File.rm_rf(root)

      if previous,
        do: Application.put_env(:exhub, :toonflow_media_client, previous),
        else: Application.delete_env(:exhub, :toonflow_media_client)
    end)

    %{server: server}
  end

  test "image_path is project-local and sanitized" do
    assert Media.image_path("/tmp/p", "sht_1") == "/tmp/p/assets/images/sht_1.png"
    assert Media.image_path("/tmp/p", "a/b") == "/tmp/p/assets/images/a_b.png"
  end

  test "generate_image from a free prompt records an asset", %{server: server} do
    assert {:ok, _} = Store.create_project("demo", [], server)

    assert {:ok, asset} = Media.generate_image("demo", [prompt: "一只猫"], server)
    assert asset["kind"] == "image"
    assert asset["url"] == "stub://一只猫"
    assert asset["prompt"] == "一只猫"
    assert asset["path"] =~ "/assets/images/"
    assert File.exists?(asset["path"])
  end

  test "generate_image with no prompt or shot errors", %{server: server} do
    assert {:ok, _} = Store.create_project("demo", [], server)
    assert {:error, :missing_prompt} = Media.generate_image("demo", [], server)
  end

  test "generate_image propagates client errors", %{server: server} do
    assert {:ok, _} = Store.create_project("demo", [], server)
    assert {:error, {:image_failed, :boom}} = Media.generate_image("demo", [prompt: "x"], server)
  end
end

defmodule Exhub.Toonflow.MediaTest.StubClient do
  @moduledoc false
  @behaviour Exhub.Toonflow.Media.Client

  @impl true
  def generate_image(prompt, opts) do
    if prompt == "x" do
      {:error, :boom}
    else
      path = Keyword.fetch!(opts, :out_path)
      File.mkdir_p!(Path.dirname(path))
      File.write!(path, "PNG")

      {:ok,
       %{
         "path" => path,
         "url" => "stub://" <> prompt,
         "model" => opts[:model],
         "size" => opts[:size]
       }}
    end
  end
end
