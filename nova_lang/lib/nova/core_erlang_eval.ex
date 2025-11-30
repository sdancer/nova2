defmodule Nova.CoreErlangEval do
  @moduledoc """
  Compile and evaluate Core Erlang code dynamically within the running BEAM VM.

  This allows Nova to compile source code to Core Erlang and execute it
  without spawning a new BEAM instance.
  """

  @doc """
  Compile Core Erlang source code and load it as a module.

  Returns {:ok, module_name} on success, or {:error, reason} on failure.
  """
  def compile_and_load(core_erlang_source) when is_binary(core_erlang_source) do
    compile_and_load(String.to_charlist(core_erlang_source))
  end

  def compile_and_load(core_erlang_source) when is_list(core_erlang_source) do
    with {:ok, tokens, _} <- :core_scan.string(core_erlang_source),
         {:ok, core_ast} <- :core_parse.parse(tokens),
         {:ok, mod_name, binary_code} <- :compile.forms(core_ast, [:from_core, :binary]),
         {:module, ^mod_name} <- :code.load_binary(mod_name, ~c"dynamic", binary_code) do
      {:ok, mod_name}
    else
      {:error, reason, _} -> {:error, {:scan_error, reason}}
      {:error, reason} -> {:error, {:parse_error, reason}}
      error -> {:error, error}
    end
  end

  @doc """
  Compile Core Erlang source and call a function in the resulting module.

  Returns {:ok, result} on success, or {:error, reason} on failure.
  """
  def eval(core_erlang_source, function \\ :main, args \\ []) do
    case compile_and_load(core_erlang_source) do
      {:ok, mod_name} ->
        try do
          result = apply(mod_name, function, args)
          {:ok, result}
        rescue
          e -> {:error, {:runtime_error, e}}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Compile Nova source code to Core Erlang and evaluate it.

  This is the high-level function that takes Nova/PureScript source,
  compiles it to Core Erlang, and runs it.
  """
  def eval_nova(nova_source, function \\ :main, args \\ []) do
    # Tokenize
    tokens = Nova.Compiler.Tokenizer.tokenize(nova_source)

    # Parse
    case Nova.Compiler.Parser.parse_module(tokens) do
      {:left, error} ->
        {:error, {:parse_error, error}}

      {:right, {module, _remaining}} ->
        # Generate Core Erlang
        # Note: We need to add CodeGenCoreErlang to the compiled modules
        core_erlang = generate_core_erlang(module)

        # Compile and eval
        eval(core_erlang, function, args)
    end
  end

  # Generate Core Erlang from AST
  # For now, this is a placeholder - we need to compile CodeGenCoreErlang.purs
  defp generate_core_erlang(_module) do
    # TODO: Call Nova.Compiler.CodeGenCoreErlang.gen_module/1
    raise "CodeGenCoreErlang not yet compiled to Elixir"
  end
end
