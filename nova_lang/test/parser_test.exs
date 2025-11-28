# Parser Tests - mirrors test/parser/ParserTest.purs
# Run with: mix run test/parser_test.exs

defmodule ParserTest do
  def run do
    IO.puts("=== Parser Tests (Stage 2 Elixir) ===\n")

    results = [
      # Test 1: Module header
      test_case("Module header", "module Main where", fn result ->
        case result do
          {:right, {:tuple, {:decl_module, m}, _}} -> m.name == "Main"
          _ -> false
        end
      end),

      # Test 2: Simple import
      test_case("Simple import", "import Prelude", fn result ->
        case result do
          {:right, {:tuple, {:decl_import, i}, _}} -> i.module_name == "Prelude"
          _ -> false
        end
      end),

      # Test 3: Import with alias
      test_case("Import with alias", "import Data.Array as A", fn result ->
        case result do
          {:right, {:tuple, {:decl_import, i}, _}} ->
            i.module_name == "Data.Array" and i.alias_ == {:just, "A"}
          _ -> false
        end
      end),

      # Test 4: Data type
      test_case("Data type", "data Maybe a = Nothing | Just a", fn result ->
        case result do
          {:right, {:tuple, {:decl_data_type, d}, _}} ->
            d.name == "Maybe" and length(d.constructors) == 2
          _ -> false
        end
      end),

      # Test 5: Type alias
      test_case("Type alias", "type Pair a = { fst :: a, snd :: a }", fn result ->
        case result do
          {:right, {:tuple, {:decl_type_alias, a}, _}} -> a.name == "Pair"
          _ -> false
        end
      end),

      # Test 6: Simple function
      test_case("Simple function", "add x y = x + y", fn result ->
        case result do
          {:right, {:tuple, {:decl_function, f}, _}} ->
            f.name == "add" and length(f.parameters) == 2
          _ -> false
        end
      end),

      # Test 7: Lambda expression
      test_expr("Lambda", "\\x -> x + 1", fn expr ->
        case expr do
          {:expr_lambda, params, _} -> length(params) == 1
          _ -> false
        end
      end),

      # Test 8: If expression
      test_expr("If expression", "if true then 1 else 0", fn expr ->
        case expr do
          {:expr_if, _, _, _} -> true
          _ -> false
        end
      end),

      # Test 9: Let expression
      test_expr("Let expression", "let x = 1 in x + 1", fn expr ->
        case expr do
          {:expr_let, bindings, _} -> length(bindings) == 1
          _ -> false
        end
      end),

      # Test 10: Function application
      test_expr("Function application", "f x y", fn expr ->
        case expr do
          {:expr_app, {:expr_app, _, _}, _} -> true
          _ -> false
        end
      end),

      # Test 11: Binary operators
      test_expr("Binary operators", "1 + 2 * 3", fn expr ->
        case expr do
          {:expr_bin_op, "+", _, _} -> true
          _ -> false
        end
      end),

      # Test 12: List literal
      test_expr("List literal", "[1, 2, 3]", fn expr ->
        case expr do
          {:expr_list, elems} -> length(elems) == 3
          _ -> false
        end
      end),

      # Test 13: Record literal
      test_expr("Record literal", "{ x: 1, y: 2 }", fn expr ->
        case expr do
          {:expr_record, fields} -> length(fields) == 2
          _ -> false
        end
      end),

      # Test 14: Qualified identifier
      test_expr("Qualified identifier", "Array.length", fn expr ->
        case expr do
          {:expr_qualified, "Array", "length"} -> true
          _ -> false
        end
      end),

      # Test 15: Case expression
      test_expr("Case expression", "case x of\n  Nothing -> 0\n  Just n -> n", fn expr ->
        case expr do
          {:expr_case, _, clauses} -> length(clauses) == 2
          _ -> false
        end
      end),

      # Test 16: Type signature parsing
      test_case("Type signature", "foo :: Int -> String", fn result ->
        case result do
          {:right, {:tuple, {:decl_type_sig, ts}, _}} -> ts.name == "foo"
          _ -> false
        end
      end),

      # Test 17: Function type
      test_type("Function type", "Int -> String", fn ty ->
        case ty do
          {:ty_expr_arrow, _, _} -> true
          _ -> false
        end
      end),

      # Test 18: Parameterized type
      test_type("Parameterized type", "Maybe Int", fn ty ->
        case ty do
          {:ty_expr_app, {:ty_expr_con, "Maybe"}, {:ty_expr_con, "Int"}} -> true
          _ -> false
        end
      end),

      # Test 19: Record type
      test_type("Record type", "{ name :: String, age :: Int }", fn ty ->
        case ty do
          {:ty_expr_record, fields, _} -> length(fields) == 2
          _ -> false
        end
      end),

      # Test 20: Full module
      test_module("Full module", """
module Test where

import Prelude

foo :: Int -> Int
foo x = x + 1
""", fn m ->
        m.name == "Test" and length(m.declarations) >= 2
      end),

      # Test 21: Backtick infix
      test_expr("Backtick infix", "x `elem` [1,2,3]", fn expr ->
        case expr do
          {:expr_app, {:expr_app, _, _}, _} -> true
          _ -> false
        end
      end),

      # Test 22: Do notation
      test_expr("Do notation", "do\n  x <- getLine\n  pure x", fn expr ->
        case expr do
          {:expr_do, stmts} -> length(stmts) == 2
          _ -> false
        end
      end),

      # Test 23: Pattern matching in case
      test_expr("Constructor pattern", "case x of\n  Just (Tuple a b) -> a", fn expr ->
        case expr do
          {:expr_case, _, clauses} -> length(clauses) == 1
          _ -> false
        end
      end),

      # Test 24: Guarded function
      test_case("Guarded function", "abs x\n  | x < 0 = -x\n  | otherwise = x", fn result ->
        case result do
          {:right, {:tuple, {:decl_function, f}, _}} ->
            f.name == "abs" and length(f.guards) == 2
          _ -> false
        end
      end),

      # Test 25: Derive instance
      test_case("Derive instance", "derive instance eqMaybe :: Eq a => Eq (Maybe a)", fn result ->
        case result do
          {:right, {:tuple, {:decl_type_class_instance, _}, _}} -> true
          _ -> false
        end
      end)
    ]

    passed = Enum.count(results, & &1)
    failed = length(results) - passed

    IO.puts("\n=== Results ===")
    IO.puts("Passed: #{passed}/#{length(results)}")
    if failed > 0, do: IO.puts("Failed: #{failed}")
    IO.puts("")

    if failed == 0 do
      IO.puts("✓ All tests passed!")
    else
      IO.puts("✗ Some tests failed")
      System.halt(1)
    end
  end

  defp test_case(name, input, check) do
    tokens = Nova.FastTokenizer.tokenize(input)
    result = Nova.Compiler.Parser.parse_declaration(tokens)
    if check.(result) do
      IO.puts("✓ #{name}")
      true
    else
      IO.puts("✗ #{name}")
      IO.puts("  Input: #{inspect(input)}")
      case result do
        {:left, err} -> IO.puts("  Error: #{err}")
        {:right, {:tuple, decl, _}} -> IO.puts("  Got: #{elem(decl, 0)}")
      end
      false
    end
  end

  defp test_expr(name, input, check) do
    tokens = Nova.FastTokenizer.tokenize(input)
    result = Nova.Compiler.Parser.parse_expression(tokens)
    case result do
      {:right, {:tuple, expr, _}} ->
        if check.(expr) do
          IO.puts("✓ #{name}")
          true
        else
          IO.puts("✗ #{name}")
          IO.puts("  Input: #{inspect(input)}")
          IO.puts("  Expression parsed but check failed: #{elem(expr, 0)}")
          false
        end
      {:left, err} ->
        IO.puts("✗ #{name}")
        IO.puts("  Input: #{inspect(input)}")
        IO.puts("  Error: #{err}")
        false
    end
  end

  defp test_type(name, input, check) do
    tokens = Nova.FastTokenizer.tokenize(input)
    result = Nova.Compiler.Parser.parse_type(tokens)
    case result do
      {:right, {:tuple, ty, _}} ->
        if check.(ty) do
          IO.puts("✓ #{name}")
          true
        else
          IO.puts("✗ #{name}")
          IO.puts("  Input: #{inspect(input)}")
          IO.puts("  Type parsed but check failed: #{inspect(ty)}")
          false
        end
      {:left, err} ->
        IO.puts("✗ #{name}")
        IO.puts("  Input: #{inspect(input)}")
        IO.puts("  Error: #{err}")
        false
    end
  end

  defp test_module(name, input, check) do
    tokens = Nova.FastTokenizer.tokenize(input)
    result = Nova.Compiler.Parser.parse_module(tokens)
    case result do
      {:right, {:tuple, m, _}} ->
        if check.(m) do
          IO.puts("✓ #{name}")
          true
        else
          IO.puts("✗ #{name}")
          IO.puts("  Input: #{inspect(input)}")
          IO.puts("  Module parsed but check failed")
          false
        end
      {:left, err} ->
        IO.puts("✗ #{name}")
        IO.puts("  Input: #{inspect(input)}")
        IO.puts("  Error: #{err}")
        false
    end
  end
end

ParserTest.run()
