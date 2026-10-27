defmodule Exhub.KuriDaemonTest do
  use ExUnit.Case, async: true

  alias Exhub.KuriDaemon

  @profile "/Users/edmondfrank/.kuri/chrome-profile-headless"
  @chrome "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"

  describe "parse_orphaned_profile_chrome_pids/2" do
    test "reaps an orphaned managed Chrome bound to kuri's profile" do
      ps = """
          1541     1 #{@chrome} --user-data-dir=#{@profile} --headless=new --remote-debugging-port=9222
      """

      assert KuriDaemon.parse_orphaned_profile_chrome_pids(ps, @profile) == [1541]
    end

    test "keeps a Chrome whose live kuri parent still owns it" do
      ps = """
          79810 79807 #{@chrome} --user-data-dir=#{@profile} --headless=new
      """

      assert KuriDaemon.parse_orphaned_profile_chrome_pids(ps, @profile) == []
    end

    test "keeps Chrome helper processes spawned by a live Chrome" do
      ps = """
          79810 79807 #{@chrome} --user-data-dir=#{@profile}
          79816 79810 Google Chrome Helper (Renderer) --type=renderer --user-data-dir=#{@profile} --remote-debugging-port=9222
      """

      assert KuriDaemon.parse_orphaned_profile_chrome_pids(ps, @profile) == []
    end

    test "ignores an orphaned Chrome using a different profile" do
      ps = """
          1541     1 #{@chrome} --user-data-dir=/Users/edmondfrank/Library/Application Support/Google/Chrome
      """

      assert KuriDaemon.parse_orphaned_profile_chrome_pids(ps, @profile) == []
    end

    test "the non-headless profile dir does not match a headless Chrome" do
      headless = "/Users/edmondfrank/.kuri/chrome-profile-headless"
      non_headless = "/Users/edmondfrank/.kuri/chrome-profile"

      ps = """
          1541     1 #{@chrome} --user-data-dir=#{headless} --headless=new
      """

      assert KuriDaemon.parse_orphaned_profile_chrome_pids(ps, non_headless) == []
      assert KuriDaemon.parse_orphaned_profile_chrome_pids(ps, headless) == [1541]
    end

    test "ignores a non-Chrome process that merely mentions the profile" do
      ps = """
          9999     1 /bin/sh -c echo --user-data-dir=#{@profile}
      """

      assert KuriDaemon.parse_orphaned_profile_chrome_pids(ps, @profile) == []
    end

    test "ignores malformed lines and empty output" do
      ps = """
         notapid     1 #{@chrome} --user-data-dir=#{@profile}
          1234 abc #{@chrome} --user-data-dir=#{@profile}
          12
      """

      assert KuriDaemon.parse_orphaned_profile_chrome_pids(ps, @profile) == []
      assert KuriDaemon.parse_orphaned_profile_chrome_pids("", @profile) == []
    end
  end
end
