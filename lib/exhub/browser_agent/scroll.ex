defmodule Exhub.BrowserAgent.Scroll do
  @moduledoc """
  Scrolls the element that actually scrolls the page.

  `window.scrollBy/2` is a no-op on container-scrolling pages — DevDocs scrolls
  its `<main>` pane, not the document — so the script uses the document when it
  scrolls, otherwise the main content pane (`main`, `[role=main]`, `article`),
  and otherwise the most visible scrollable element (a scrolling sidebar is
  earlier in the document but does not drive the article). It returns the
  scroller's new offset as a *string*, so the HTTP backend does not have to
  JSON-encode a result the way `window.scrollBy` (which returns `undefined`)
  forces.
  """

  @doc """
  JavaScript that scrolls one viewport in `direction` (`:up` or `:down`) and
  returns the scroller's new `scrollTop`.
  """
  @spec script(:up | :down) :: String.t()
  def script(direction) when direction in [:up, :down] do
    delta = if direction == :up, do: "-", else: ""

    """
    (function () {
      var amount = #{delta}window.innerHeight;

      function scrollable(el) {
        return el.clientHeight > 0 && el.scrollHeight > el.clientHeight + 4;
      }

      var el = document.scrollingElement || document.documentElement;

      if (!scrollable(el)) {
        // Prefer the page's main content pane, so a scrolling navigation
        // sidebar earlier in the document is not mistaken for the article.
        var candidate = null;
        var preferred = document.querySelectorAll("main, [role=main], article");

        for (var i = 0; i < preferred.length; i++) {
          if (scrollable(preferred[i])) { candidate = preferred[i]; break; }
        }

        if (!candidate) {
          // Otherwise the most visible scrollable element (largest viewport
          // area), which skips the window-level scrollers already rejected.
          var nodes = document.querySelectorAll("div, section");
          var best = 0;

          for (var j = 0; j < nodes.length; j++) {
            var node = nodes[j];
            if (!scrollable(node)) continue;
            var area = node.clientWidth * node.clientHeight;
            if (area > best) { best = area; candidate = node; }
          }
        }

        if (candidate) el = candidate;
      }

      el.scrollTop += amount;
      return String(el.scrollTop);
    })()
    """
  end
end
