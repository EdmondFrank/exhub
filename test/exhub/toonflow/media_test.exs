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

  describe "refs_for_shot/3" do
    setup do
      dir = Path.join(System.tmp_dir!(), "toonflow_refs_#{System.unique_integer([:positive])}")
      File.mkdir_p!(dir)
      on_exit(fn -> File.rm_rf(dir) end)
      %{dir: dir}
    end

    test "orders explicit → scene → character and drops missing files", %{dir: dir} do
      explicit = Path.join(dir, "explicit.png")
      scene_ref = Path.join(dir, "scene.png")
      character_ref = Path.join(dir, "char.png")
      missing = Path.join(dir, "missing.png")

      for path <- [explicit, scene_ref, character_ref], do: File.write!(path, "x")

      shot = %{"scene" => "祖宅", "characters" => ["林昭"], "meta" => %{}}

      ctx = %{
        "scenes" => [%{"name" => "祖宅", "refs" => [scene_ref, missing]}],
        "characters" => [%{"name" => "林昭", "refs" => [character_ref]}]
      }

      assert Media.refs_for_shot(shot, ctx, refs: [explicit, scene_ref]) ==
               [explicit, scene_ref, character_ref]
    end

    test "a selected variation reference wins over the base reference", %{dir: dir} do
      base = Path.join(dir, "base.png")
      variation = Path.join(dir, "variation.png")
      File.write!(base, "x")
      File.write!(variation, "x")

      shot = %{
        "characters" => ["林昭"],
        "meta" => %{"character_variations" => %{"林昭" => "var_1"}}
      }

      ctx = %{
        "characters" => [%{"name" => "林昭", "refs" => [base]}],
        "variations" => %{"var_1" => %{"refs" => [variation]}}
      }

      assert Media.refs_for_shot(shot, ctx) == [variation]

      # An unknown variation id (or a variation with no refs) falls back to base.
      other = put_in(shot, ["meta", "character_variations", "林昭"], "var_missing")
      assert Media.refs_for_shot(other, ctx) == [base]
    end

    test "tolerates a nil shot and an empty context" do
      assert Media.refs_for_shot(nil, %{}) == []
      assert Media.refs_for_shot(%{"scene" => "x"}, %{}) == []
    end
  end

  describe "variation_prompt/1" do
    test "prefixes the keep-the-face instruction" do
      prompt = Media.variation_prompt("黑色夜行衣")

      assert prompt =~ "仅更换服装"
      assert String.ends_with?(prompt, "黑色夜行衣")
    end
  end

  describe "candidate grids" do
    test "grid_shape/1 picks a near-square layout" do
      assert Media.grid_shape(9) == {3, 3}
      assert Media.grid_shape(4) == {2, 2}
      assert Media.grid_shape(16) == {4, 4}
      assert Media.grid_shape(1) == {1, 1}
    end

    test "candidates_prompt/4 asks for a cols×rows grid" do
      prompt = Media.candidates_prompt("青衫少年立于门前", 3, 3, "统一冷色调")

      assert prompt =~ "3×3"
      assert prompt =~ "青衫少年立于门前"
      assert prompt =~ "统一冷色调"

      plain = Media.candidates_prompt("青衫少年立于门前", 3, 3)
      assert plain =~ "3×3"
      refute plain =~ "额外要求"
    end

    test "crop_filter/3 and crop_argv/6 are pure" do
      assert Media.crop_filter(0, 3, 3) == "crop=iw/3:ih/3:0*iw/3:0*ih/3"
      assert Media.crop_filter(4, 3, 3) == "crop=iw/3:ih/3:1*iw/3:1*ih/3"
      assert Media.crop_filter(8, 3, 3) == "crop=iw/3:ih/3:2*iw/3:2*ih/3"
      assert Media.crop_filter(5, 3, 2) == "crop=iw/3:ih/2:2*iw/3:1*ih/2"

      assert Media.crop_argv("ffmpeg", "in.png", 4, 3, 3, "out.png") == [
               "ffmpeg",
               "-y",
               "-i",
               "in.png",
               "-vf",
               "crop=iw/3:ih/3:1*iw/3:1*ih/3",
               "-frames:v",
               "1",
               "out.png"
             ]
    end

    test "generate_candidates/3 records a grid image", %{server: server} do
      assert {:ok, _} = Store.create_project("demo", [], server)

      assert {:ok, asset} = Media.generate_candidates("demo", [prompt: "门前", count: 9], server)

      assert asset["kind"] == Media.candidates_kind()
      assert asset["meta"]["grid"] == %{"cols" => 3, "rows" => 3}
      assert asset["meta"]["cells"] == 9
      assert asset["meta"]["status"] == "completed"
      assert asset["path"] =~ "_candidates.png"
      assert asset["prompt"] =~ "3×3"
      assert File.exists?(asset["path"])
    end

    test "generate_candidates/3 needs a prompt or a shot", %{server: server} do
      assert {:ok, _} = Store.create_project("demo", [], server)
      assert {:error, :missing_prompt} = Media.generate_candidates("demo", [], server)
    end
  end

  describe "edits_form/4" do
    setup do
      dir = Path.join(System.tmp_dir!(), "toonflow_edits_#{System.unique_integer([:positive])}")
      File.mkdir_p!(dir)
      path = Path.join(dir, "ref.png")
      File.write!(path, <<137, 80, 78, 71>>)
      scene = Path.join(dir, "scene.png")
      File.write!(scene, <<137, 80, 78, 72>>)
      on_exit(fn -> File.rm_rf(dir) end)
      %{path: path, scene: scene}
    end

    test "sends one image part per reference, in order", %{path: path, scene: scene} do
      form = Media.Default.edits_form("draw a ninja", "gpt-image-2", "1536x1024", [scene, path])

      assert Keyword.get(form, :model) == "gpt-image-2"
      assert Keyword.get(form, :prompt) == "draw a ninja"
      assert Keyword.get(form, :response_format) == "url"
      assert Keyword.get(form, :size) == "1536x1024"

      # The scene comes first (it is the base the model preserves), then the
      # character sheet — gpt-image-2 only keeps the referenced cast when both
      # arrive in the same request.
      images = for {:image, {bytes, opts}} <- form, do: {opts[:filename], bytes, opts}

      assert [{"scene.png", <<137, 80, 78, 72>>, scene_opts}, {"ref.png", bytes, opts}] =
               images

      assert scene_opts[:content_type] == "image/png"
      assert bytes == <<137, 80, 78, 71>>
      assert opts[:content_type] == "image/png"
    end

    test "keeps the size for models that do not restrict it", %{path: path} do
      form = Media.Default.edits_form("draw", "FLUX.1-Kontext-dev", "1024x1024", [path])
      assert Keyword.get(form, :size) == "1024x1024"
    end

    test "the form encodes and consumes to a valid multipart body", %{path: path, scene: scene} do
      form = Media.Default.edits_form("draw", "gpt-image-2", "1024x1024", [scene, path])

      assert %{content_type: "multipart/form-data;" <> _, size: size, body: body} =
               Req.Utils.encode_form_multipart(form)

      assert size > 0

      encoded = body |> Enum.to_list() |> IO.iodata_to_binary()
      assert encoded =~ ~s(name="model")
      assert encoded =~ ~s(name="prompt")
      assert encoded =~ ~s(name="size")
      assert encoded =~ ~s(filename="scene.png")
      assert encoded =~ ~s(filename="ref.png")
      assert encoded =~ "content-type: image/png"
    end

    test "a `{:bytes, …}` part is not encodable — Req reads it as a stream", %{path: _path} do
      %{body: body} =
        Req.Utils.encode_form_multipart([{:image, {:bytes, "ref.png", <<1>>, []}}])

      # It only blows up when the (lazily built) body is consumed, which is why
      # this reached production as a mid-request crash.
      assert_raise Protocol.UndefinedError, fn ->
        body |> Enum.to_list() |> IO.iodata_to_binary()
      end
    end
  end

  describe "gpt_size/1" do
    test "passes through a size the endpoint accepts" do
      for size <- ~w(1024x1024 1536x1024 1024x1536) do
        assert Media.Default.gpt_size(size) == size
      end
    end

    test "snaps an unsupported size to the closest orientation" do
      # Verified live: the endpoint rejects 1792x1024 with `400 参数无效 'size'`.
      assert Media.Default.gpt_size("1792x1024") == "1536x1024"
      assert Media.Default.gpt_size("1024x1792") == "1024x1536"
      assert Media.Default.gpt_size("800x600") == "1536x1024"
      assert Media.Default.gpt_size("600x800") == "1024x1536"
      assert Media.Default.gpt_size("512x512") == "1024x1024"
    end

    test "falls back to square for anything unparseable" do
      assert Media.Default.gpt_size("1024*1024") == "1024x1024"
      assert Media.Default.gpt_size("banana") == "1024x1024"
      assert Media.Default.gpt_size(nil) == "1024x1024"
    end
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
