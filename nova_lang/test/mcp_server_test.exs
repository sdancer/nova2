defmodule Nova.MCPServerTest do
  use ExUnit.Case, async: true

  alias Nova.MCPServer

  setup do
    {:ok, state} = MCPServer.init([])
    %{state: state}
  end

  describe "initialization" do
    test "init starts namespace service", %{state: state} do
      assert is_pid(state.namespace_service)
    end

    test "handle_initialize returns server info", %{state: state} do
      {:ok, result, _state} = MCPServer.handle_initialize(%{}, state)
      assert result.server_info.name == "nova-compiler"
      assert result.server_info.version == "0.1.0"
      assert result.capabilities.tools == %{}
    end
  end

  describe "tool listing" do
    test "lists all available tools", %{state: state} do
      {:ok, tools, _state} = MCPServer.handle_list_tools(state)
      tool_names = Enum.map(tools, & &1.name)

      # Namespace management
      assert "create_namespace" in tool_names
      assert "delete_namespace" in tool_names
      assert "list_namespaces" in tool_names

      # Declaration management
      assert "add_declaration" in tool_names
      assert "update_declaration" in tool_names
      assert "remove_declaration" in tool_names
      assert "list_declarations" in tool_names
      assert "get_declaration" in tool_names

      # Type checking
      assert "validate_namespace" in tool_names
      assert "get_type" in tool_names
      assert "get_diagnostics" in tool_names

      # Code intelligence
      assert "get_completions" in tool_names

      # Imports
      assert "add_import" in tool_names
      assert "list_imports" in tool_names
    end

    test "each tool has required schema fields", %{state: state} do
      {:ok, tools, _state} = MCPServer.handle_list_tools(state)

      for tool <- tools do
        assert Map.has_key?(tool, :name)
        assert Map.has_key?(tool, :description)
        assert Map.has_key?(tool, :inputSchema)
        assert tool.inputSchema.type == "object"
      end
    end
  end

  describe "namespace CRUD" do
    test "create and list namespaces", %{state: state} do
      # Create namespace
      {:ok, [%{text: result}], state} =
        MCPServer.handle_call_tool("create_namespace", %{"name" => "Test.Module"}, state)
      assert result =~ "created successfully"

      # List namespaces
      {:ok, [%{text: result}], _state} =
        MCPServer.handle_call_tool("list_namespaces", %{}, state)
      assert result =~ "Test.Module"
    end

    test "delete namespace", %{state: state} do
      # Create then delete
      {:ok, _, state} =
        MCPServer.handle_call_tool("create_namespace", %{"name" => "ToDelete"}, state)
      {:ok, [%{text: result}], state} =
        MCPServer.handle_call_tool("delete_namespace", %{"name" => "ToDelete"}, state)
      assert result =~ "deleted successfully"

      # Verify it's gone
      {:ok, [%{text: result}], _state} =
        MCPServer.handle_call_tool("list_namespaces", %{}, state)
      refute result =~ "ToDelete"
    end

    test "create duplicate namespace returns error", %{state: state} do
      {:ok, _, state} =
        MCPServer.handle_call_tool("create_namespace", %{"name" => "Duplicate"}, state)
      {:ok, [%{text: result}], _state} =
        MCPServer.handle_call_tool("create_namespace", %{"name" => "Duplicate"}, state)
      assert result =~ "Error"
      assert result =~ "already exists"
    end
  end

  describe "declaration CRUD" do
    test "add and list declarations", %{state: state} do
      # Create namespace first
      {:ok, _, state} =
        MCPServer.handle_call_tool("create_namespace", %{"name" => "Test"}, state)

      # Add declaration
      {:ok, [%{text: result}], state} =
        MCPServer.handle_call_tool("add_declaration", %{
          "namespace" => "Test",
          "source" => "add x y = x + y"
        }, state)
      assert result =~ "decl_id"

      # List declarations
      {:ok, [%{text: result}], _state} =
        MCPServer.handle_call_tool("list_declarations", %{"namespace" => "Test"}, state)
      assert result =~ "add"
    end

    test "get declaration by name", %{state: state} do
      {:ok, _, state} =
        MCPServer.handle_call_tool("create_namespace", %{"name" => "Test"}, state)
      {:ok, _, state} =
        MCPServer.handle_call_tool("add_declaration", %{
          "namespace" => "Test",
          "source" => "multiply a b = a * b"
        }, state)

      {:ok, [%{text: result}], _state} =
        MCPServer.handle_call_tool("get_declaration", %{
          "namespace" => "Test",
          "name" => "multiply"
        }, state)
      assert result =~ "multiply"
      assert result =~ "source"
    end

    test "update declaration", %{state: state} do
      {:ok, _, state} =
        MCPServer.handle_call_tool("create_namespace", %{"name" => "Test"}, state)
      {:ok, [%{text: add_result}], state} =
        MCPServer.handle_call_tool("add_declaration", %{
          "namespace" => "Test",
          "source" => "foo x = x"
        }, state)

      # Extract decl_id from JSON response
      {:ok, parsed} = Jason.decode(add_result)
      decl_id = parsed["decl_id"]

      {:ok, [%{text: result}], _state} =
        MCPServer.handle_call_tool("update_declaration", %{
          "decl_id" => decl_id,
          "source" => "foo x = x + 1"
        }, state)
      assert result =~ "updated successfully"
    end

    test "remove declaration", %{state: state} do
      {:ok, _, state} =
        MCPServer.handle_call_tool("create_namespace", %{"name" => "Test"}, state)
      {:ok, [%{text: add_result}], state} =
        MCPServer.handle_call_tool("add_declaration", %{
          "namespace" => "Test",
          "source" => "toRemove x = x"
        }, state)

      {:ok, parsed} = Jason.decode(add_result)
      decl_id = parsed["decl_id"]

      {:ok, [%{text: result}], _state} =
        MCPServer.handle_call_tool("remove_declaration", %{"decl_id" => decl_id}, state)
      assert result =~ "removed successfully"
    end
  end

  describe "type checking" do
    test "validate namespace with valid code", %{state: state} do
      {:ok, _, state} =
        MCPServer.handle_call_tool("create_namespace", %{"name" => "Test"}, state)
      {:ok, _, state} =
        MCPServer.handle_call_tool("add_declaration", %{
          "namespace" => "Test",
          "source" => "add :: Int -> Int -> Int\nadd x y = x + y"
        }, state)

      {:ok, [%{text: result}], _state} =
        MCPServer.handle_call_tool("validate_namespace", %{"namespace" => "Test"}, state)
      assert result =~ "validated successfully"
    end

    test "get diagnostics for namespace", %{state: state} do
      {:ok, _, state} =
        MCPServer.handle_call_tool("create_namespace", %{"name" => "Test"}, state)
      {:ok, _, state} =
        MCPServer.handle_call_tool("add_declaration", %{
          "namespace" => "Test",
          "source" => "inc x = x + 1"
        }, state)

      # Validate first to populate diagnostics
      {:ok, _, state} =
        MCPServer.handle_call_tool("validate_namespace", %{"namespace" => "Test"}, state)

      {:ok, [%{text: result}], _state} =
        MCPServer.handle_call_tool("get_diagnostics", %{"namespace" => "Test"}, state)
      # Should be empty array for valid code
      assert result == "[]"
    end
  end

  describe "imports" do
    test "add and list imports", %{state: state} do
      {:ok, _, state} =
        MCPServer.handle_call_tool("create_namespace", %{"name" => "Lib"}, state)
      {:ok, _, state} =
        MCPServer.handle_call_tool("create_namespace", %{"name" => "App"}, state)

      {:ok, [%{text: result}], state} =
        MCPServer.handle_call_tool("add_import", %{
          "namespace" => "App",
          "imported_namespace" => "Lib"
        }, state)
      assert result =~ "Import added"

      {:ok, [%{text: result}], _state} =
        MCPServer.handle_call_tool("list_imports", %{"namespace" => "App"}, state)
      assert result =~ "Lib"
    end
  end
end
