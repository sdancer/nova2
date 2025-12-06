defmodule InfixOperatorTest do
  use ExUnit.Case, async: true

  describe "$ operator (function application)" do
    test "$ produces same code as parenthesized application" do
      code1 = """
      module Test where
      test1 x = show $ x + 1
      """

      code2 = """
      module Test where
      test2 x = show (x + 1)
      """

      {:ok, result1} = Nova.compile(code1)
      {:ok, result2} = Nova.compile(code2)

      # Extract just the function body (ignore function name difference)
      body1 = extract_function_body(result1, "test1")
      body2 = extract_function_body(result2, "test2")

      assert body1 == body2, """
      $ operator should produce same code as parentheses
      With $: #{body1}
      With (): #{body2}
      """
    end

    test "$ with qualified function call" do
      code1 = """
      module Test where
      import Data.Array as Array
      test1 xs = Array.fromFoldable $ map show xs
      """

      code2 = """
      module Test where
      import Data.Array as Array
      test2 xs = Array.fromFoldable (map show xs)
      """

      {:ok, result1} = Nova.compile(code1)
      {:ok, result2} = Nova.compile(code2)

      body1 = extract_function_body(result1, "test1")
      body2 = extract_function_body(result2, "test2")

      assert body1 == body2
    end

    test "nested $ operators" do
      # Define add locally so the test doesn't fail on unbound variable
      code = """
      module Test where
      add a b = a + b
      test1 x = show $ add 1 $ x + 2
      """

      {:ok, result} = Nova.compile(code)
      # Should compile without error and produce proper nested calls
      assert result =~ "Nova.Runtime.show"
    end
  end

  describe "# operator (reverse function application)" do
    test "# produces reverse application" do
      code = """
      module Test where
      test1 x = x # show
      """

      {:ok, result} = Nova.compile(code)
      # x # show is equivalent to show x
      assert result =~ "Nova.Runtime.show(x)"
    end
  end

  describe "<<< and >>> operators (composition)" do
    @tag :skip
    test "<<< composes right to left" do
      # Composition operators require Prelude/Semigroupoid
      code = """
      module Test where
      add1 x = x + 1
      test1 = show <<< add1
      """

      {:ok, result} = Nova.compile(code)
      assert result =~ "test1"
    end

    @tag :skip
    test ">>> composes left to right" do
      # Composition operators require Prelude/Semigroupoid
      code = """
      module Test where
      add1 x = x + 1
      test1 = add1 >>> show
      """

      {:ok, result} = Nova.compile(code)
      assert result =~ "test1"
    end
  end

  describe "arithmetic operators" do
    test "+ addition" do
      code = """
      module Test where
      test1 x y = x + y
      """

      {:ok, result} = Nova.compile(code)
      assert result =~ "(x + y)"
    end

    test "- subtraction" do
      code = """
      module Test where
      test1 x y = x - y
      """

      {:ok, result} = Nova.compile(code)
      assert result =~ "(x - y)"
    end

    test "* multiplication" do
      code = """
      module Test where
      test1 x y = x * y
      """

      {:ok, result} = Nova.compile(code)
      assert result =~ "(x * y)"
    end

    test "/ division" do
      code = """
      module Test where
      test1 x y = x / y
      """

      {:ok, result} = Nova.compile(code)
      assert result =~ "(x / y)"
    end
  end

  describe "comparison operators" do
    test "== equality" do
      code = """
      module Test where
      test1 x y = x == y
      """

      {:ok, result} = Nova.compile(code)
      assert result =~ "(x == y)"
    end

    test "/= inequality" do
      code = """
      module Test where
      test1 x y = x /= y
      """

      {:ok, result} = Nova.compile(code)
      assert result =~ "(x != y)" or result =~ "not"
    end

    test "< less than" do
      code = """
      module Test where
      test1 x y = x < y
      """

      {:ok, result} = Nova.compile(code)
      assert result =~ "(x < y)"
    end

    test "<= less than or equal" do
      code = """
      module Test where
      test1 x y = x <= y
      """

      {:ok, result} = Nova.compile(code)
      assert result =~ "(x <= y)"
    end

    test "> greater than" do
      code = """
      module Test where
      test1 x y = x > y
      """

      {:ok, result} = Nova.compile(code)
      assert result =~ "(x > y)"
    end

    test ">= greater than or equal" do
      code = """
      module Test where
      test1 x y = x >= y
      """

      {:ok, result} = Nova.compile(code)
      assert result =~ "(x >= y)"
    end
  end

  describe "boolean operators" do
    test "&& and" do
      code = """
      module Test where
      test1 x y = x && y
      """

      {:ok, result} = Nova.compile(code)
      assert result =~ "(x && y)" or result =~ "(x and y)"
    end

    test "|| or" do
      code = """
      module Test where
      test1 x y = x || y
      """

      {:ok, result} = Nova.compile(code)
      assert result =~ "(x || y)" or result =~ "(x or y)"
    end
  end

  describe "list operators" do
    test ": cons" do
      code = """
      module Test where
      test1 x xs = x : xs
      """

      {:ok, result} = Nova.compile(code)
      assert result =~ "[x | xs]"
    end

    test "<> append" do
      code = """
      module Test where
      test1 xs ys = xs <> ys
      """

      {:ok, result} = Nova.compile(code)
      assert result =~ "append" or result =~ "++"
    end
  end

  describe "monadic operators" do
    @tag :skip
    test ">>= bind" do
      # Requires Monad/Bind typeclass from Prelude
      code = """
      module Test where
      test1 m f = m >>= f
      """

      {:ok, result} = Nova.compile(code)
      assert result =~ "bind"
    end

    @tag :skip
    test "<$> map/fmap" do
      # Requires Functor typeclass from Prelude
      code = """
      module Test where
      test1 f xs = f <$> xs
      """

      {:ok, result} = Nova.compile(code)
      assert result =~ "map"
    end

    @tag :skip
    test "<*> apply" do
      # Requires Applicative typeclass from Prelude
      code = """
      module Test where
      test1 f xs = f <*> xs
      """

      {:ok, result} = Nova.compile(code)
      assert result =~ "apply"
    end
  end

  # Helper to extract function body from generated code
  defp extract_function_body(code, fn_name) do
    # Match "def fn_name(...) do\n    <body>\n  end"
    regex = ~r/def #{fn_name}\([^)]*\) do\s*\n\s*(.+?)\s*\n\s*end/s
    case Regex.run(regex, code) do
      [_, body] -> String.trim(body)
      _ -> nil
    end
  end
end
