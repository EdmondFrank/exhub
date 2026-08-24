defmodule Exhub.MCP.DesktopServerTest do
  use ExUnit.Case, async: true

  describe "module loading" do
    test "DesktopServer module exists" do
      assert is_atom(Exhub.MCP.DesktopServer)
      functions = Exhub.MCP.DesktopServer.__info__(:functions)
      assert {:init, 2} in functions
      assert {:handle_request, 2} in functions
    end

    test "Helpers module exists" do
      assert is_atom(Exhub.MCP.Desktop.Helpers)
      functions = Exhub.MCP.Desktop.Helpers.__info__(:functions)
      assert {:toon_response, 2} in functions
      assert {:expand_path, 1} in functions
      assert {:validate_absolute_path, 1} in functions
      assert {:clean_env, 0} in functions
      assert {:get_shell, 0} in functions
      assert {:shell_command_args, 1} in functions
      assert {:shell_command_args, 2} in functions
      assert {:needs_working_dir?, 1} in functions
    end

    test "ProcessStore module exists" do
      assert is_atom(Exhub.MCP.Desktop.ProcessStore)
      functions = Exhub.MCP.Desktop.ProcessStore.__info__(:functions)
      assert {:register, 2} in functions
      assert {:get, 1} in functions
      assert {:append_output, 2} in functions
      assert {:send_input, 2} in functions
      assert {:list, 0} in functions
    end

    test "ExecListener module exists" do
      assert is_atom(Exhub.MCP.Desktop.ExecListener)
      functions = Exhub.MCP.Desktop.ExecListener.__info__(:functions)
      assert {:run, 4} in functions
    end

    test "PortListener module exists" do
      assert is_atom(Exhub.MCP.Desktop.PortListener)
      functions = Exhub.MCP.Desktop.PortListener.__info__(:functions)
      assert {:loop, 2} in functions
    end
  end

  describe "filesystem tool modules" do
    test "ReadFile module exists" do
      assert is_atom(Exhub.MCP.Tools.Desktop.ReadFile)
      functions = Exhub.MCP.Tools.Desktop.ReadFile.__info__(:functions)
      assert {:name, 0} in functions
      assert {:description, 0} in functions
      assert {:execute, 2} in functions
    end

    test "ReadMultipleFiles module exists" do
      assert is_atom(Exhub.MCP.Tools.Desktop.ReadMultipleFiles)
      functions = Exhub.MCP.Tools.Desktop.ReadMultipleFiles.__info__(:functions)
      assert {:name, 0} in functions
      assert {:description, 0} in functions
      assert {:execute, 2} in functions
    end

    test "WriteFile module exists" do
      assert is_atom(Exhub.MCP.Tools.Desktop.WriteFile)
      functions = Exhub.MCP.Tools.Desktop.WriteFile.__info__(:functions)
      assert {:name, 0} in functions
      assert {:description, 0} in functions
      assert {:execute, 2} in functions
    end

    test "CreateDirectory module exists" do
      assert is_atom(Exhub.MCP.Tools.Desktop.CreateDirectory)
      functions = Exhub.MCP.Tools.Desktop.CreateDirectory.__info__(:functions)
      assert {:name, 0} in functions
      assert {:description, 0} in functions
      assert {:execute, 2} in functions
    end

    test "ListDirectory module exists" do
      assert is_atom(Exhub.MCP.Tools.Desktop.ListDirectory)
      functions = Exhub.MCP.Tools.Desktop.ListDirectory.__info__(:functions)
      assert {:name, 0} in functions
      assert {:description, 0} in functions
      assert {:execute, 2} in functions
    end

    test "MoveFile module exists" do
      assert is_atom(Exhub.MCP.Tools.Desktop.MoveFile)
      functions = Exhub.MCP.Tools.Desktop.MoveFile.__info__(:functions)
      assert {:name, 0} in functions
      assert {:description, 0} in functions
      assert {:execute, 2} in functions
    end

    test "GetFileInfo module exists" do
      assert is_atom(Exhub.MCP.Tools.Desktop.GetFileInfo)
      functions = Exhub.MCP.Tools.Desktop.GetFileInfo.__info__(:functions)
      assert {:name, 0} in functions
      assert {:description, 0} in functions
      assert {:execute, 2} in functions
    end

    test "DeleteFile module exists" do
      assert is_atom(Exhub.MCP.Tools.Desktop.DeleteFile)
      functions = Exhub.MCP.Tools.Desktop.DeleteFile.__info__(:functions)
      assert {:name, 0} in functions
      assert {:description, 0} in functions
      assert {:execute, 2} in functions
    end
  end

  describe "edit tool modules" do
    test "EditBlock module exists" do
      assert is_atom(Exhub.MCP.Tools.Desktop.EditBlock)
      functions = Exhub.MCP.Tools.Desktop.EditBlock.__info__(:functions)
      assert {:name, 0} in functions
      assert {:description, 0} in functions
      assert {:execute, 2} in functions
    end
  end

  describe "process/terminal tool modules" do
    test "ExecuteCommand module exists" do
      assert is_atom(Exhub.MCP.Tools.Desktop.ExecuteCommand)
      functions = Exhub.MCP.Tools.Desktop.ExecuteCommand.__info__(:functions)
      assert {:name, 0} in functions
      assert {:description, 0} in functions
      assert {:execute, 2} in functions
    end

    test "StartProcess module exists" do
      assert is_atom(Exhub.MCP.Tools.Desktop.StartProcess)
      functions = Exhub.MCP.Tools.Desktop.StartProcess.__info__(:functions)
      assert {:name, 0} in functions
      assert {:description, 0} in functions
      assert {:execute, 2} in functions
    end

    test "ReadProcessOutput module exists" do
      assert is_atom(Exhub.MCP.Tools.Desktop.ReadProcessOutput)
      functions = Exhub.MCP.Tools.Desktop.ReadProcessOutput.__info__(:functions)
      assert {:name, 0} in functions
      assert {:description, 0} in functions
      assert {:execute, 2} in functions
    end

    test "InteractWithProcess module exists" do
      assert is_atom(Exhub.MCP.Tools.Desktop.InteractWithProcess)
      functions = Exhub.MCP.Tools.Desktop.InteractWithProcess.__info__(:functions)
      assert {:name, 0} in functions
      assert {:description, 0} in functions
      assert {:execute, 2} in functions
    end

    test "ListManagedProcesses module exists" do
      assert is_atom(Exhub.MCP.Tools.Desktop.ListManagedProcesses)
      functions = Exhub.MCP.Tools.Desktop.ListManagedProcesses.__info__(:functions)
      assert {:name, 0} in functions
      assert {:description, 0} in functions
      assert {:execute, 2} in functions
    end

    test "ListProcesses module exists" do
      assert is_atom(Exhub.MCP.Tools.Desktop.ListProcesses)
      functions = Exhub.MCP.Tools.Desktop.ListProcesses.__info__(:functions)
      assert {:name, 0} in functions
      assert {:description, 0} in functions
      assert {:execute, 2} in functions
    end

    test "KillProcess module exists" do
      assert is_atom(Exhub.MCP.Tools.Desktop.KillProcess)
      functions = Exhub.MCP.Tools.Desktop.KillProcess.__info__(:functions)
      assert {:name, 0} in functions
      assert {:description, 0} in functions
      assert {:execute, 2} in functions
    end
  end

  describe "search tool modules" do
    test "SearchFiles module exists" do
      assert is_atom(Exhub.MCP.Tools.Desktop.SearchFiles)
      functions = Exhub.MCP.Tools.Desktop.SearchFiles.__info__(:functions)
      assert {:name, 0} in functions
      assert {:description, 0} in functions
      assert {:execute, 2} in functions
    end
  end

  describe "tool names" do
    test "ReadFile has correct name" do
      assert Exhub.MCP.Tools.Desktop.ReadFile.name() == "read_file"
    end

    test "ReadMultipleFiles has correct name" do
      assert Exhub.MCP.Tools.Desktop.ReadMultipleFiles.name() == "read_multiple_files"
    end

    test "WriteFile has correct name" do
      assert Exhub.MCP.Tools.Desktop.WriteFile.name() == "write_file"
    end

    test "CreateDirectory has correct name" do
      assert Exhub.MCP.Tools.Desktop.CreateDirectory.name() == "create_directory"
    end

    test "ListDirectory has correct name" do
      assert Exhub.MCP.Tools.Desktop.ListDirectory.name() == "list_directory"
    end

    test "MoveFile has correct name" do
      assert Exhub.MCP.Tools.Desktop.MoveFile.name() == "move_file"
    end

    test "GetFileInfo has correct name" do
      assert Exhub.MCP.Tools.Desktop.GetFileInfo.name() == "get_file_info"
    end

    test "DeleteFile has correct name" do
      assert Exhub.MCP.Tools.Desktop.DeleteFile.name() == "delete_file"
    end

    test "EditBlock has correct name" do
      assert Exhub.MCP.Tools.Desktop.EditBlock.name() == "edit_block"
    end

    test "ExecuteCommand has correct name" do
      assert Exhub.MCP.Tools.Desktop.ExecuteCommand.name() == "execute_command"
    end

    test "StartProcess has correct name" do
      assert Exhub.MCP.Tools.Desktop.StartProcess.name() == "start_process"
    end

    test "ReadProcessOutput has correct name" do
      assert Exhub.MCP.Tools.Desktop.ReadProcessOutput.name() == "read_process_output"
    end

    test "InteractWithProcess has correct name" do
      assert Exhub.MCP.Tools.Desktop.InteractWithProcess.name() == "interact_with_process"
    end

    test "ListManagedProcesses has correct name" do
      assert Exhub.MCP.Tools.Desktop.ListManagedProcesses.name() == "list_managed_processes"
    end

    test "ListProcesses has correct name" do
      assert Exhub.MCP.Tools.Desktop.ListProcesses.name() == "list_processes"
    end

    test "KillProcess has correct name" do
      assert Exhub.MCP.Tools.Desktop.KillProcess.name() == "kill_process"
    end

    test "SearchFiles has correct name" do
      assert Exhub.MCP.Tools.Desktop.SearchFiles.name() == "search_files"
    end
  end

  describe "tool descriptions" do
    test "ReadFile has description" do
      description = Exhub.MCP.Tools.Desktop.ReadFile.description()
      assert is_binary(description)
      assert String.length(description) > 0
    end

    test "ReadMultipleFiles has description" do
      description = Exhub.MCP.Tools.Desktop.ReadMultipleFiles.description()
      assert is_binary(description)
      assert String.length(description) > 0
    end

    test "WriteFile has description" do
      description = Exhub.MCP.Tools.Desktop.WriteFile.description()
      assert is_binary(description)
      assert String.length(description) > 0
    end

    test "CreateDirectory has description" do
      description = Exhub.MCP.Tools.Desktop.CreateDirectory.description()
      assert is_binary(description)
      assert String.length(description) > 0
    end

    test "ListDirectory has description" do
      description = Exhub.MCP.Tools.Desktop.ListDirectory.description()
      assert is_binary(description)
      assert String.length(description) > 0
    end

    test "MoveFile has description" do
      description = Exhub.MCP.Tools.Desktop.MoveFile.description()
      assert is_binary(description)
      assert String.length(description) > 0
    end

    test "GetFileInfo has description" do
      description = Exhub.MCP.Tools.Desktop.GetFileInfo.description()
      assert is_binary(description)
      assert String.length(description) > 0
    end

    test "DeleteFile has description" do
      description = Exhub.MCP.Tools.Desktop.DeleteFile.description()
      assert is_binary(description)
      assert String.length(description) > 0
    end

    test "EditBlock has description" do
      description = Exhub.MCP.Tools.Desktop.EditBlock.description()
      assert is_binary(description)
      assert String.length(description) > 0
    end

    test "ExecuteCommand has description" do
      description = Exhub.MCP.Tools.Desktop.ExecuteCommand.description()
      assert is_binary(description)
      assert String.length(description) > 0
    end

    test "StartProcess has description" do
      description = Exhub.MCP.Tools.Desktop.StartProcess.description()
      assert is_binary(description)
      assert String.length(description) > 0
    end

    test "ReadProcessOutput has description" do
      description = Exhub.MCP.Tools.Desktop.ReadProcessOutput.description()
      assert is_binary(description)
      assert String.length(description) > 0
    end

    test "InteractWithProcess has description" do
      description = Exhub.MCP.Tools.Desktop.InteractWithProcess.description()
      assert is_binary(description)
      assert String.length(description) > 0
    end

    test "ListManagedProcesses has description" do
      description = Exhub.MCP.Tools.Desktop.ListManagedProcesses.description()
      assert is_binary(description)
      assert String.length(description) > 0
    end

    test "ListProcesses has description" do
      description = Exhub.MCP.Tools.Desktop.ListProcesses.description()
      assert is_binary(description)
      assert String.length(description) > 0
    end

    test "KillProcess has description" do
      description = Exhub.MCP.Tools.Desktop.KillProcess.description()
      assert is_binary(description)
      assert String.length(description) > 0
    end

    test "SearchFiles has description" do
      description = Exhub.MCP.Tools.Desktop.SearchFiles.description()
      assert is_binary(description)
      assert String.length(description) > 0
    end
  end

  describe "component count" do
    test "DesktopServer has 17 component declarations" do
      # Count the component() calls by extracting the module body
      {:ok, content} = File.read("lib/exhub/mcp/desktop_server.ex")
      component_count =
        content
        |> String.split("\n")
        |> Enum.count(&String.contains?(&1, "component("))
      assert component_count == 17,
             "Expected 17 component() declarations, got #{component_count}"
    end
  end
end