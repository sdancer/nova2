# BEAM-native regenerate script
# Equivalent to: node scripts/regenerate-purs.js
# Loads Core Erlang modules and calls Nova.Regenerate.main()

# Change to project root (nova_lang's parent directory)
project_root = Path.join(__DIR__, "..")
File.cd!(project_root)

# Directory containing compiled .core files
core_dir = "nova_lang/priv/core"

# Load a .core file and compile it into the BEAM VM
load_core_module = fn path ->
  case File.read(path) do
    {:ok, content} ->
      case Nova.CoreErlangEval.compile_and_load(content) do
        {:ok, mod_name} -> {:ok, mod_name}
        {:error, reason} -> {:error, path, reason}
      end
    {:error, reason} ->
      {:error, path, reason}
  end
end

# Modules needed by Nova.Regenerate (in dependency order)
lib_modules = [
  "Control/Lazy.core",
  "Data/Maybe.core",
  "Data/Tuple.core",
  "Data/Either.core",
  "Data/Char.core",
  "Data/Int.core",
  "Data/Number.core",
  "Data/Array.core",
  "Data/List.core",
  "Data/String.core",
  "Data/String/CodeUnits.core",
  "Data/Foldable.core",
  "Data/Map.core",
  "Data/Set.core",
  "Nova/Prelude.core"
]

compiler_modules = [
  "Nova/Compiler/Ast.core",
  "Nova/Compiler/Types.core",
  "Nova/Compiler/Cst.core",
  "Nova/Compiler/CstLayout.core",
  "Nova/Compiler/CstLexer.core",
  "Nova/Compiler/CstParser.core",
  "Nova/Compiler/CstToAst.core",
  "Nova/Compiler/CstPipeline.core",
  "Nova/Compiler/Unify.core",
  "Nova/Compiler/ImportProcessor.core",
  "Nova/Compiler/TypeChecker.core",
  "Nova/Compiler/CodeGenCoreErlang.core",
  "Nova/Regenerate.core"
]

all_modules = lib_modules ++ compiler_modules

# Load all modules silently
errors = Enum.reduce(all_modules, [], fn rel_path, errors ->
  path = Path.join(core_dir, rel_path)
  case load_core_module.(path) do
    {:ok, _mod_name} -> errors
    {:error, path, reason} ->
      IO.puts("FAILED to load #{path}: #{inspect(reason)}")
      [rel_path | errors]
  end
end)

if errors != [] do
  IO.puts("\nCannot proceed - #{length(errors)} module(s) failed to load")
  System.halt(1)
end

# Call the BEAM-native regenerate
:'Nova.Regenerate'.main(:unit)
