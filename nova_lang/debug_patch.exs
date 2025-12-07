defmodule DebugPatch do
  # Copy of patch_imports from run_purs_passing_tests.exs
  def patch_imports(code) do
    code
    # Effect.Console imports
    # (log).(x) -> (&Nova.Runtime.log/1).(x) - for function reference application
    |> String.replace(~r/\(log\)\.\(/, "(&Nova.Runtime.log/1).(")
    # log.("Done") -> Nova.Runtime.log("Done") - for direct call
    |> String.replace(~r/(?<!Runtime\.)(?<!&)log\.\(/, "Nova.Runtime.log(")
    |> String.replace(~r/\(logShow\)\.\(/, "(&Nova.Runtime.log_show/1).(")
    |> String.replace(~r/(?<!Runtime\.)log_show\.\(/, "Nova.Runtime.log_show(")
    |> String.replace(~r/(?<!Runtime\.)logShow\.\(/, "Nova.Runtime.log_show(")
    # Data.Show imports
    |> String.replace(~r/\(show\)\.\(/, "(&Nova.Runtime.show/1).(")
    |> String.replace(~r/(?<!Runtime\.)(?<!&)show\.\(/, "Nova.Runtime.show(")
    # Control.Assert / Test.Assert imports
    # Both camelCase (from imports) and snake_case (from codegen) patterns
    |> String.replace(~r/(?<!Runtime\.)assert\.\(/, "Nova.Runtime.assert(")
    |> String.replace(~r/(?<!Runtime\.)assertEqual\.\(/, "Nova.Runtime.assert_equal(")
    |> String.replace(~r/(?<!Runtime\.)assert_equal\.\(/, "Nova.Runtime.assert_equal(")
    |> String.replace(~r/(?<!Runtime\.)assertPrime\.\(/, "Nova.Runtime.assert_prime(")
    |> String.replace(~r/(?<!Runtime\.)assert_prime\.\(/, "Nova.Runtime.assert_prime(")
    |> String.replace(~r/(?<!Runtime\.)assertThrows\.\(/, "Nova.Runtime.assert_throws(")
    |> String.replace(~r/(?<!Runtime\.)assert_throws\.\(/, "Nova.Runtime.assert_throws(")
    # Data.Function imports
    |> String.replace(~r/(?<!Runtime\.)flip\.\(/, "Nova.Runtime.flip(")
    |> String.replace(~r/(?<!Runtime\.)const\.\(/, "Nova.Runtime.const_(")
    |> String.replace(~r/(?<!Runtime\.)identity\.\(/, "Nova.Runtime.identity(")
    # Prelude/basic ops
    |> String.replace(~r/(?<!Runtime\.)negate\.\(/, "Nova.Runtime.negate(")
    |> String.replace(~r/(?<!Runtime\.)coerce\.\(/, "Nova.Runtime.coerce(")
    # Unit
    |> String.replace(~r/(?<!Runtime\.)\bunit\b(?!\.)/, "Nova.Runtime.unit()")
    # FFI functions (Data.Function.Uncurried, Partial.Unsafe)
    |> String.replace(~r/(?<!Runtime\.)unsafePartial\.\(/, "Nova.Runtime.unsafe_partial(")
    |> String.replace(~r/(?<!Runtime\.)unsafe_partial\.\(/, "Nova.Runtime.unsafe_partial(")
    |> String.replace(~r/(?<!Runtime\.)runFn2\.\(/, "Nova.Runtime.run_fn2(")
    |> String.replace(~r/(?<!Runtime\.)run_fn2\.\(/, "Nova.Runtime.run_fn2(")
    |> String.replace(~r/(?<!Runtime\.)mkFn2\.\(/, "Nova.Runtime.mk_fn2(")
    |> String.replace(~r/(?<!Runtime\.)mk_fn2\.\(/, "Nova.Runtime.mk_fn2(")
    # Type class methods that may be imported from Prelude
    |> String.replace(~r/\(pure\)\.\(/, "(&Nova.Runtime.pure/1).(")
    |> String.replace(~r/(?<!Runtime\.)pure\.\(/, "Nova.Runtime.pure(")
    |> String.replace(~r/\(bind\)\.\(/, "(&Nova.Runtime.bind/2).(")
    |> String.replace(~r/(?<!Runtime\.)bind\.\(/, "Nova.Runtime.bind(")
    |> String.replace(~r/\(map\)\.\(/, "(&Nova.Runtime.map/2).(")
    |> String.replace(~r/(?<!Runtime\.)(?<!Enum\.)map\.\(/, "Nova.Runtime.map(")
    |> String.replace(~r/\(apply\)\.\(/, "(&Nova.Runtime.apply/2).(")
    |> String.replace(~r/(?<!Runtime\.)apply\.\(/, "Nova.Runtime.apply(")
  end
end

source = File.read!("purs_tests/Console.purs")
{:right, ast} = Nova.Compiler.CstPipeline.parse_module_cst(source)
code = Nova.Compiler.CodeGen.gen_module().(ast)

IO.puts("=== Before patch ===")
IO.puts(code)
IO.puts("\n=== After patch ===")
patched = DebugPatch.patch_imports(code)
IO.puts(patched)

# Get line 14
lines = String.split(patched, "\n")
IO.puts("\n=== Line 14 ===")
IO.puts(Enum.at(lines, 13) || "No line 14")

# Try to compile and get the actual error
IO.puts("\n=== Compile attempt ===")
try do
  Code.compile_string(patched)
  IO.puts("Compiled OK")
rescue
  e -> IO.puts("Error: #{Exception.message(e)}")
end
