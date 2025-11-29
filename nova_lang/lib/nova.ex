defmodule Nova do
  @moduledoc """
  Nova Lang - A PureScript-like language that compiles to Elixir.

  This is the self-hosted compiler running on the BEAM VM.
  """

  @doc """
  Compile a Nova source file to Elixir code.

  Returns `{:ok, elixir_code}` on success, or `{:error, reason}` on failure.
  """
  def compile_file(path, opts \\ []) do
    deps = Keyword.get(opts, :deps, [])

    with {:ok, source} <- File.read(path),
         {:ok, elixir_code} <- compile(source, deps) do
      {:ok, elixir_code}
    end
  end

  @doc """
  Compile Nova source code to Elixir code.
  """
  def compile(source, dep_sources \\ []) do
    # Parse dependencies first
    dep_decls = Enum.flat_map(dep_sources, fn dep_src ->
      tokens = Nova.Compiler.Tokenizer.tokenize(dep_src)
      case Nova.Compiler.Parser.parse_module(tokens) do
        {:left, _} -> []
        {:right, {:tuple, mod, _rest}} -> mod.declarations
      end
    end)

    # Tokenize
    tokens = Nova.Compiler.Tokenizer.tokenize(source)

    # Parse
    case Nova.Compiler.Parser.parse_module(tokens) do
      {:left, err} ->
        {:error, {:parse, err}}

      {:right, {:tuple, mod, _rest}} ->
        # Type check
        all_decls = dep_decls ++ mod.declarations
        env = Nova.Compiler.Types.empty_env()

        case Nova.Compiler.TypeChecker.check_module(env, all_decls) do
          {:left, err} ->
            {:error, {:typecheck, err}}

          {:right, _env} ->
            # Generate Elixir code
            elixir_code = Nova.Compiler.CodeGen.gen_module(mod)
            {:ok, elixir_code}
        end
    end
  end

  @doc """
  Compile a pre-parsed module AST to Elixir code.
  """
  def compile_module(mod, dep_decls \\ []) do
    # Type check
    all_decls = dep_decls ++ mod.declarations
    env = Nova.Compiler.Types.empty_env()

    case Nova.Compiler.TypeChecker.check_module(env, all_decls) do
      {:left, err} ->
        {:error, {:typecheck, err}}

      {:right, _env} ->
        # Generate Elixir code
        elixir_code = Nova.Compiler.CodeGen.gen_module(mod)
        {:ok, elixir_code}
    end
  end

  @doc """
  Compile and write to output file.
  """
  def compile_to_file(input_path, output_path, opts \\ []) do
    case compile_file(input_path, opts) do
      {:ok, code} ->
        File.write(output_path, code)

      error ->
        error
    end
  end
end
