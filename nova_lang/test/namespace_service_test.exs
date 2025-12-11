defmodule Nova.NamespaceServiceTest do
  use ExUnit.Case, async: true

  alias Nova.NamespaceService

  describe "namespace management" do
    test "create and list namespaces" do
      {:ok, svc} = NamespaceService.start_link()

      assert :ok = NamespaceService.create_namespace(svc, "MyApp.Core")
      assert {:ok, namespaces} = NamespaceService.list_namespaces(svc)
      assert "MyApp.Core" in namespaces
    end

    test "cannot create duplicate namespace" do
      {:ok, svc} = NamespaceService.start_link()

      assert :ok = NamespaceService.create_namespace(svc, "MyApp.Core")
      assert {:error, :already_exists} = NamespaceService.create_namespace(svc, "MyApp.Core")
    end

    test "delete namespace" do
      {:ok, svc} = NamespaceService.start_link()

      :ok = NamespaceService.create_namespace(svc, "MyApp.Core")
      assert :ok = NamespaceService.delete_namespace(svc, "MyApp.Core")
      assert {:ok, []} = NamespaceService.list_namespaces(svc)
    end
  end

  describe "declaration management" do
    test "add function declaration" do
      {:ok, svc} = NamespaceService.start_link()
      :ok = NamespaceService.create_namespace(svc, "Test")

      {:ok, decl_id} = NamespaceService.add_declaration(svc, "Test", "add x y = x + y")
      assert decl_id =~ "Test:function:add"
    end

    test "add data type declaration" do
      {:ok, svc} = NamespaceService.start_link()
      :ok = NamespaceService.create_namespace(svc, "Test")

      {:ok, decl_id} = NamespaceService.add_declaration(svc, "Test", "data Color = Red | Green | Blue")
      assert decl_id =~ "Test:datatype:Color"
    end

    test "list declarations" do
      {:ok, svc} = NamespaceService.start_link()
      :ok = NamespaceService.create_namespace(svc, "Test")

      {:ok, _} = NamespaceService.add_declaration(svc, "Test", "add x y = x + y")
      {:ok, _} = NamespaceService.add_declaration(svc, "Test", "mul x y = x * y")

      {:ok, decls} = NamespaceService.list_declarations(svc, "Test")
      assert length(decls) == 2
      names = Enum.map(decls, & &1.name)
      assert "add" in names
      assert "mul" in names
    end

    test "cannot add duplicate name in namespace" do
      {:ok, svc} = NamespaceService.start_link()
      :ok = NamespaceService.create_namespace(svc, "Test")

      {:ok, _} = NamespaceService.add_declaration(svc, "Test", "add x y = x + y")
      assert {:error, {:duplicate_name, "add"}} =
        NamespaceService.add_declaration(svc, "Test", "add a b = a + b + 1")
    end

    test "remove declaration" do
      {:ok, svc} = NamespaceService.start_link()
      :ok = NamespaceService.create_namespace(svc, "Test")

      {:ok, decl_id} = NamespaceService.add_declaration(svc, "Test", "add x y = x + y")
      assert :ok = NamespaceService.remove_declaration(svc, decl_id)

      {:ok, decls} = NamespaceService.list_declarations(svc, "Test")
      assert decls == []
    end

    test "update declaration" do
      {:ok, svc} = NamespaceService.start_link()
      :ok = NamespaceService.create_namespace(svc, "Test")

      {:ok, decl_id} = NamespaceService.add_declaration(svc, "Test", "add x y = x + y")
      {:ok, ^decl_id} = NamespaceService.update_declaration(svc, decl_id, "add a b = a + b + 1")

      # Check version was bumped
      {:ok, managed} = NamespaceService.get_declaration(svc, "Test", "add")
      assert managed.meta.version == 2
    end
  end

  describe "status tracking" do
    test "new declarations have fresh status" do
      {:ok, svc} = NamespaceService.start_link()
      :ok = NamespaceService.create_namespace(svc, "Test")

      {:ok, decl_id} = NamespaceService.add_declaration(svc, "Test", "add x y = x + y")
      {:ok, status} = NamespaceService.get_status(svc, decl_id)
      assert status == :fresh
    end
  end

  describe "completions" do
    test "get completions by prefix" do
      {:ok, svc} = NamespaceService.start_link()
      :ok = NamespaceService.create_namespace(svc, "Test")

      {:ok, _} = NamespaceService.add_declaration(svc, "Test", "addOne x = x + 1")
      {:ok, _} = NamespaceService.add_declaration(svc, "Test", "addTwo x = x + 2")
      {:ok, _} = NamespaceService.add_declaration(svc, "Test", "multiply x y = x * y")

      {:ok, completions} = NamespaceService.get_completions(svc, "Test", "add")
      assert length(completions) == 2
      names = Enum.map(completions, & &1.name)
      assert "addOne" in names
      assert "addTwo" in names
    end
  end

  describe "type checking" do
    test "validate_namespace type checks declarations" do
      {:ok, svc} = NamespaceService.start_link()
      :ok = NamespaceService.create_namespace(svc, "Test")

      {:ok, id1} = NamespaceService.add_declaration(svc, "Test", "add x y = x + y")
      {:ok, id2} = NamespaceService.add_declaration(svc, "Test", "double x = x * 2")

      # Before validation, status is fresh
      {:ok, status} = NamespaceService.get_status(svc, id1)
      assert status == :fresh

      # Validate the namespace
      {:ok, count} = NamespaceService.validate_namespace(svc, "Test")
      assert count == 2

      # After validation, status should be valid
      {:ok, status} = NamespaceService.get_status(svc, id1)
      assert status == :valid

      {:ok, status} = NamespaceService.get_status(svc, id2)
      assert status == :valid
    end

    test "get_type returns inferred type after validation" do
      {:ok, svc} = NamespaceService.start_link()
      :ok = NamespaceService.create_namespace(svc, "Test")

      {:ok, decl_id} = NamespaceService.add_declaration(svc, "Test", "identity x = x")

      # Before validation, get_type returns not_validated error
      {:error, {:not_validated, :fresh}} = NamespaceService.get_type(svc, decl_id)

      # Validate
      {:ok, _} = NamespaceService.validate_namespace(svc, "Test")

      # After validation, get_type returns the inferred type
      {:ok, type} = NamespaceService.get_type(svc, decl_id)
      assert type != nil
    end

    test "validate_namespace with data types" do
      {:ok, svc} = NamespaceService.start_link()
      :ok = NamespaceService.create_namespace(svc, "Test")

      {:ok, _} = NamespaceService.add_declaration(svc, "Test", "data Color = Red | Green | Blue")
      {:ok, _} = NamespaceService.add_declaration(svc, "Test", """
        isRed c = case c of
          Red -> true
          _ -> false
      """)

      {:ok, count} = NamespaceService.validate_namespace(svc, "Test")
      assert count == 2

      {:ok, diags} = NamespaceService.get_diagnostics(svc, "Test")
      assert diags == []
    end

    test "validate_namespace reports type errors" do
      {:ok, svc} = NamespaceService.start_link()
      :ok = NamespaceService.create_namespace(svc, "Test")

      # This should type check fine
      {:ok, _} = NamespaceService.add_declaration(svc, "Test", "add x y = x + y")

      # This references undefined function
      {:ok, _} = NamespaceService.add_declaration(svc, "Test", "bad = undefinedFunc 42")

      # Validation should fail
      result = NamespaceService.validate_namespace(svc, "Test")
      assert {:error, {:type_errors, _}} = result
    end
  end

  describe "cross-namespace imports" do
    test "add and list imports" do
      {:ok, svc} = NamespaceService.start_link()
      :ok = NamespaceService.create_namespace(svc, "Lib")
      :ok = NamespaceService.create_namespace(svc, "App")

      # Add import
      assert :ok = NamespaceService.add_import(svc, "App", "Lib")

      # List imports
      {:ok, imports} = NamespaceService.list_imports(svc, "App")
      assert "Lib" in imports
    end

    test "cannot import non-existent namespace" do
      {:ok, svc} = NamespaceService.start_link()
      :ok = NamespaceService.create_namespace(svc, "App")

      assert {:error, {:import_not_found, "NonExistent"}} =
        NamespaceService.add_import(svc, "App", "NonExistent")
    end

    test "cannot create circular imports" do
      {:ok, svc} = NamespaceService.start_link()
      :ok = NamespaceService.create_namespace(svc, "A")
      :ok = NamespaceService.create_namespace(svc, "B")

      :ok = NamespaceService.add_import(svc, "A", "B")

      # B importing A would create a cycle
      assert {:error, {:circular_import, "B", "A"}} =
        NamespaceService.add_import(svc, "B", "A")
    end

    test "remove import" do
      {:ok, svc} = NamespaceService.start_link()
      :ok = NamespaceService.create_namespace(svc, "Lib")
      :ok = NamespaceService.create_namespace(svc, "App")

      :ok = NamespaceService.add_import(svc, "App", "Lib")
      :ok = NamespaceService.remove_import(svc, "App", "Lib")

      {:ok, imports} = NamespaceService.list_imports(svc, "App")
      assert imports == []
    end

    test "use imported declarations in type checking" do
      {:ok, svc} = NamespaceService.start_link()

      # Create library namespace with a helper function
      :ok = NamespaceService.create_namespace(svc, "Lib")
      {:ok, _} = NamespaceService.add_declaration(svc, "Lib", "double x = x * 2")

      # Validate library first
      {:ok, _} = NamespaceService.validate_namespace(svc, "Lib")

      # Create app namespace that imports library
      :ok = NamespaceService.create_namespace(svc, "App")
      :ok = NamespaceService.add_import(svc, "App", "Lib")

      # Use the imported function
      {:ok, _} = NamespaceService.add_declaration(svc, "App", "quadruple x = double (double x)")

      # Validation should succeed because 'double' is imported from Lib
      {:ok, _} = NamespaceService.validate_namespace(svc, "App")

      {:ok, diags} = NamespaceService.get_diagnostics(svc, "App")
      assert diags == []
    end

    test "auto-validates imported namespaces" do
      {:ok, svc} = NamespaceService.start_link()

      # Create library with a function (not validated yet)
      :ok = NamespaceService.create_namespace(svc, "Lib")
      {:ok, lib_id} = NamespaceService.add_declaration(svc, "Lib", "helper x = x + 1")

      # Create app that imports library
      :ok = NamespaceService.create_namespace(svc, "App")
      :ok = NamespaceService.add_import(svc, "App", "Lib")
      {:ok, _} = NamespaceService.add_declaration(svc, "App", "useHelper y = helper y")

      # Lib is not validated yet
      {:ok, status} = NamespaceService.get_status(svc, lib_id)
      assert status == :fresh

      # Validating App should auto-validate Lib first
      {:ok, _} = NamespaceService.validate_namespace(svc, "App")

      # Now Lib should be validated too
      {:ok, status} = NamespaceService.get_status(svc, lib_id)
      assert status == :valid
    end

    test "use qualified names from imports" do
      {:ok, svc} = NamespaceService.start_link()

      # Create library namespace with a helper function
      :ok = NamespaceService.create_namespace(svc, "Lib")
      {:ok, _} = NamespaceService.add_declaration(svc, "Lib", "triple x = x * 3")

      # Validate library first
      {:ok, _} = NamespaceService.validate_namespace(svc, "Lib")

      # Create app namespace that imports library
      :ok = NamespaceService.create_namespace(svc, "App")
      :ok = NamespaceService.add_import(svc, "App", "Lib")

      # Use the imported function with qualified name
      {:ok, _} = NamespaceService.add_declaration(svc, "App", "sextuple x = Lib.triple (Lib.triple x)")

      # Validation should succeed because 'Lib.triple' is available
      {:ok, _} = NamespaceService.validate_namespace(svc, "App")

      {:ok, diags} = NamespaceService.get_diagnostics(svc, "App")
      assert diags == []
    end

    test "import data type constructors" do
      {:ok, svc} = NamespaceService.start_link()

      # Create library namespace with a data type
      :ok = NamespaceService.create_namespace(svc, "Types")
      {:ok, _} = NamespaceService.add_declaration(svc, "Types", "data Status = Active | Inactive | Pending")

      # Validate library first
      {:ok, _} = NamespaceService.validate_namespace(svc, "Types")

      # Create app namespace that imports the data type
      :ok = NamespaceService.create_namespace(svc, "App")
      :ok = NamespaceService.add_import(svc, "App", "Types")

      # Use the imported constructors
      {:ok, _} = NamespaceService.add_declaration(svc, "App", """
        getStatus x = case x of
          Active -> 1
          Inactive -> 0
          Pending -> 2
      """)

      # Validation should succeed
      {:ok, _} = NamespaceService.validate_namespace(svc, "App")

      {:ok, diags} = NamespaceService.get_diagnostics(svc, "App")
      assert diags == []
    end

    test "transitive imports" do
      {:ok, svc} = NamespaceService.start_link()

      # Create chain: Core -> Lib -> App
      :ok = NamespaceService.create_namespace(svc, "Core")
      {:ok, _} = NamespaceService.add_declaration(svc, "Core", "inc x = x + 1")

      :ok = NamespaceService.create_namespace(svc, "Lib")
      :ok = NamespaceService.add_import(svc, "Lib", "Core")
      {:ok, _} = NamespaceService.add_declaration(svc, "Lib", "double x = inc (inc x) - 1")

      :ok = NamespaceService.create_namespace(svc, "App")
      :ok = NamespaceService.add_import(svc, "App", "Lib")
      {:ok, _} = NamespaceService.add_declaration(svc, "App", "quad x = double (double x)")

      # Validating App should validate the entire chain
      {:ok, _} = NamespaceService.validate_namespace(svc, "App")

      {:ok, diags} = NamespaceService.get_diagnostics(svc, "App")
      assert diags == []
    end
  end

  describe "type classes" do
    test "define type class and use method" do
      {:ok, svc} = NamespaceService.start_link()
      :ok = NamespaceService.create_namespace(svc, "Test")

      # Define a simple type class
      {:ok, _} = NamespaceService.add_declaration(svc, "Test", """
        class Showable a where
          render :: a -> String
      """)

      # Define a function that uses the class method signature
      {:ok, _} = NamespaceService.add_declaration(svc, "Test", "display x = render x")

      # Validation should succeed - render is available from the class
      {:ok, _} = NamespaceService.validate_namespace(svc, "Test")

      {:ok, diags} = NamespaceService.get_diagnostics(svc, "Test")
      assert diags == []
    end

    test "type class instance methods type check" do
      {:ok, svc} = NamespaceService.start_link()
      :ok = NamespaceService.create_namespace(svc, "Test")

      # Define a data type
      {:ok, _} = NamespaceService.add_declaration(svc, "Test", "data Color = Red | Green | Blue")

      # Define instance with method implementation
      {:ok, _} = NamespaceService.add_declaration(svc, "Test", """
        instance showColor :: Show Color where
          show Red = "red"
          show Green = "green"
          show Blue = "blue"
      """)

      # Validation should succeed
      {:ok, _} = NamespaceService.validate_namespace(svc, "Test")

      {:ok, diags} = NamespaceService.get_diagnostics(svc, "Test")
      assert diags == []
    end
  end

  describe "code generation" do
    test "generate simple function" do
      code = """
      add x y = x + y
      """
      tokens = Nova.Compiler.Tokenizer.tokenize(code)
      {:right, {:tuple, decl, _rest}} = Nova.Compiler.Parser.parse_declaration(tokens)

      ctx = Nova.Compiler.CodeGen.empty_ctx()
      generated = Nova.Compiler.CodeGen.gen_declaration(ctx, decl)

      assert generated =~ "def add(x, y) do"
      assert generated =~ "(x + y)"
    end

    test "generate function with case expression" do
      code = """
      length xs = case xs of
        [] -> 0
        (h : t) -> 1 + length t
      """
      tokens = Nova.Compiler.Tokenizer.tokenize(code)
      {:right, {:tuple, decl, _rest}} = Nova.Compiler.Parser.parse_declaration(tokens)

      ctx = Nova.Compiler.CodeGen.empty_ctx()
      |> Map.put(:module_funcs, Nova.Set.from_foldable(["length"]))
      |> Map.put(:func_arities, [%{name: "length", arity: 1}])

      generated = Nova.Compiler.CodeGen.gen_declaration(ctx, decl)

      assert generated =~ "def length(xs) do"
      assert generated =~ "case xs do"
      assert generated =~ "[] ->"
      # Pattern might have parens around it depending on parsing
      assert generated =~ "h | t" or generated =~ "[h | t]"
    end

    test "generate data type constructors" do
      code = "data Maybe a = Nothing | Just a"
      tokens = Nova.Compiler.Tokenizer.tokenize(code)
      {:right, {:tuple, decl, _rest}} = Nova.Compiler.Parser.parse_declaration(tokens)

      ctx = Nova.Compiler.CodeGen.empty_ctx()
      generated = Nova.Compiler.CodeGen.gen_declaration(ctx, decl)

      assert generated =~ "def nothing()"
      assert generated =~ ":nothing"
      assert generated =~ "def just(arg0)"
      assert generated =~ "{:just,"
    end

    test "generate lambda expression" do
      code = "double = \\x -> x * 2"
      tokens = Nova.Compiler.Tokenizer.tokenize(code)
      {:right, {:tuple, decl, _rest}} = Nova.Compiler.Parser.parse_declaration(tokens)

      ctx = Nova.Compiler.CodeGen.empty_ctx()
      generated = Nova.Compiler.CodeGen.gen_declaration(ctx, decl)

      # Lambda bindings are generated as named Elixir functions
      assert generated =~ "def double(x) do"
      assert generated =~ "(x * 2)"
    end

    test "generate record expression" do
      code = "point = { x: 1, y: 2 }"
      tokens = Nova.Compiler.Tokenizer.tokenize(code)
      {:right, {:tuple, decl, _rest}} = Nova.Compiler.Parser.parse_declaration(tokens)

      ctx = Nova.Compiler.CodeGen.empty_ctx()
      generated = Nova.Compiler.CodeGen.gen_declaration(ctx, decl)

      assert generated =~ "%{"
      assert generated =~ "x: 1"
      assert generated =~ "y: 2"
    end

    test "generate if expression" do
      code = "max a b = if a > b then a else b"
      tokens = Nova.Compiler.Tokenizer.tokenize(code)
      {:right, {:tuple, decl, _rest}} = Nova.Compiler.Parser.parse_declaration(tokens)

      ctx = Nova.Compiler.CodeGen.empty_ctx()
      generated = Nova.Compiler.CodeGen.gen_declaration(ctx, decl)

      assert generated =~ "if (a > b) do"
      assert generated =~ "else"
    end

    test "generate full module" do
      module = %{
        name: "Test.Example",
        declarations: [
          {:decl_function, %{
            name: "add",
            parameters: [{:pat_var, "x"}, {:pat_var, "y"}],
            body: {:expr_bin_op, "+", {:expr_var, "x"}, {:expr_var, "y"}},
            guards: [],
            type_signature: :nothing
          }}
        ]
      }

      generated = Nova.Compiler.CodeGen.gen_module().(module)

      assert generated =~ "defmodule Test.Example do"
      assert generated =~ "def add(x, y) do"
      assert generated =~ "end\n"
    end
  end
end
