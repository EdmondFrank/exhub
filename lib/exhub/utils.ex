defmodule Exhub.Utils do
  def get_repo_path(file_path) do
    file_dir = Path.dirname(to_string(file_path))

    case System.cmd("git", ["rev-parse", "--show-toplevel"], cd: file_dir) do
      {result, 0} -> {:ok, String.trim(result)}
      _ -> Exhub.send_message(~s[(message "not a valid git repo")])
    end
  end
end
