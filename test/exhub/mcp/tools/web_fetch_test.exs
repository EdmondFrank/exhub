defmodule Exhub.MCP.Tools.WebFetchTest do
  @moduledoc """
  Offline coverage for `web_fetch`: URL validation, the `file://` path, and the
  redirect plumbing.

  The HTTP *round trip* is deliberately not exercised here — it would need the
  network. Its proxy behaviour is decided by `Exhub.MCP.WebTools.Proxy`, which
  is covered directly (with an injected decider/probe) in
  `test/exhub/mcp/web_tools/proxy_test.exs`. The redirect helpers
  (`redirect_location/1`, `resolve_redirect/2`, `redirected_request/3`,
  `maybe_strip_credentials/3`) are pure and covered below.
  """

  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.WebFetch

  defp text(resp), do: Enum.map_join(resp.content, "\n", & &1["text"])

  defp write(name, content) do
    path = Path.join(System.tmp_dir!(), "web_fetch_test_#{name}_#{:rand.uniform(999_999)}")
    File.write!(path, content)
    on_exit_wrapper(path)
    path
  end

  defp on_exit_wrapper(path), do: ExUnit.Callbacks.on_exit(fn -> File.rm(path) end)

  describe "execute/2 validation" do
    test "requires a URL" do
      frame = %{}
      {:reply, resp, ^frame} = WebFetch.execute(%{}, frame)
      assert resp.isError
      assert text(resp) =~ "URL is required"
    end

    test "rejects a scheme it cannot fetch" do
      frame = %{}
      {:reply, resp, ^frame} = WebFetch.execute(%{url: "ftp://example.com/x"}, frame)
      assert resp.isError
      assert text(resp) =~ "Invalid URL"
    end

    test "documents that the proxy path is decided by Smart Decide" do
      assert WebFetch.description() =~ "Smart Decide"
    end
  end

  describe "file:// fetches" do
    test "parses HTML into text and drops scripts and styles" do
      path =
        write(
          "page.html",
          "<html><head><style>body{color:red}</style></head>" <>
            "<body><h1>Title</h1><p>Body text</p><script>SECRETSCRIPT()</script></body></html>"
        )

      frame = %{}
      {:reply, resp, ^frame} = WebFetch.execute(%{url: "file://" <> path}, frame)

      refute resp.isError
      assert resp.structured_content["success"] == true
      assert resp.structured_content["status_code"] == 200
      assert resp.structured_content["url"] == "file://" <> path
      content = resp.structured_content["content"]
      assert content =~ "Title"
      assert content =~ "Body text"
      refute content =~ "SECRETSCRIPT"
      refute content =~ "color:red"
    end

    test "leaves a non-HTML file untouched" do
      path = write("plain.txt", "<html not a document>\nsecond line")
      frame = %{}
      {:reply, resp, ^frame} = WebFetch.execute(%{url: "file://" <> path}, frame)

      refute resp.isError
      assert resp.structured_content["content"] == "<html not a document>\nsecond line"
    end

    test "a proxied field is only present when a proxy was actually used" do
      path = write("plain2.txt", "hello")
      frame = %{}
      {:reply, resp, ^frame} = WebFetch.execute(%{url: "file://" <> path}, frame)

      refute Map.has_key?(resp.structured_content, "proxy")
    end

    test "reports a missing file" do
      frame = %{}

      {:reply, resp, ^frame} =
        WebFetch.execute(
          %{url: "file:///definitely/not/here-#{:rand.uniform(999_999)}.txt"},
          frame
        )

      assert resp.isError
      assert text(resp) =~ "File not found"
    end
  end

  describe "redirect following" do
    test "redirect_location/1 reads Location case-insensitively" do
      assert WebFetch.redirect_location([{"Location", "https://b/"}]) == "https://b/"
      assert WebFetch.redirect_location([{"location", "/relative"}]) == "/relative"
      assert WebFetch.redirect_location([{"content-type", "text/html"}]) == nil
      assert WebFetch.redirect_location([]) == nil
      assert WebFetch.redirect_location(:nope) == nil
    end

    test "resolve_redirect/2 turns relative Locations absolute" do
      base = "https://example.com/a/b?q=1"

      assert WebFetch.resolve_redirect(base, "/c") == "https://example.com/c"
      assert WebFetch.resolve_redirect(base, "d") == "https://example.com/a/d"
      assert WebFetch.resolve_redirect(base, "https://other.org/x") == "https://other.org/x"
      assert WebFetch.resolve_redirect(base, "mailto:a@b") == nil
      assert WebFetch.resolve_redirect(base, nil) == nil
    end

    test "redirected_request/3 applies the method rules" do
      assert WebFetch.redirected_request(303, "POST", "body") == {"GET", nil}
      assert WebFetch.redirected_request(302, "POST", "body") == {"GET", nil}
      assert WebFetch.redirected_request(301, "POST", "body") == {"GET", nil}
      assert WebFetch.redirected_request(307, "POST", "body") == {"POST", "body"}
      assert WebFetch.redirected_request(308, "PUT", "body") == {"PUT", "body"}
      assert WebFetch.redirected_request(302, "GET", nil) == {"GET", nil}
      assert WebFetch.redirected_request(303, "HEAD", nil) == {"HEAD", nil}
    end

    test "maybe_strip_credentials/3 drops secrets only across origins" do
      headers = %{"Authorization" => "Bearer t", "Cookie" => "s=1", "Accept" => "text/html"}

      assert WebFetch.maybe_strip_credentials(headers, "https://a.com/x", "https://a.com/y") ==
               headers

      assert WebFetch.maybe_strip_credentials(headers, "https://a.com/x", "https://b.com/y") ==
               %{"Accept" => "text/html"}

      # A scheme downgrade is a different origin too.
      assert WebFetch.maybe_strip_credentials(headers, "https://a.com/x", "http://a.com/y") ==
               %{"Accept" => "text/html"}

      assert WebFetch.maybe_strip_credentials(nil, "https://a.com", "https://b.com") == nil
    end
  end
end
