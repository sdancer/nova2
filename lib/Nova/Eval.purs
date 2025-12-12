-- | Nova Eval - Compile and evaluate expressions/snippets
-- |
-- | Compiles Nova code to temporary Core Erlang modules,
-- | loads them dynamically, and evaluates expressions.
-- | Similar to Erlang's shell/REPL functionality.
module Nova.Eval where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

-- | Result of evaluation
type EvalResult a =
  { value :: a
  , moduleName :: String
  }

-- | Evaluation context tracking loaded modules
type EvalContext =
  { counter :: Int
  , modules :: Array String
  }

-- | Create a new evaluation context
newContext :: EvalContext
newContext = { counter: 0, modules: [] }

-- | Generate a unique module name
genModuleName :: EvalContext -> { name :: String, ctx :: EvalContext }
genModuleName ctx =
  let name = "Nova.Eval.M" <> intToString ctx.counter
      newCtx = ctx { counter = ctx.counter + 1 }
  in { name: name, ctx: newCtx }

foreign import intToString :: Int -> String
  = "call 'erlang':'integer_to_list'($0)"

-- | Evaluate a Nova expression string
-- | Returns the result of evaluating the expression
evalExpr :: String -> Either String String
evalExpr source =
  let modName = "Nova.Eval.Expr"
      -- Wrap expression in a module with eval function
      moduleSource = "module " <> modName <> " where\n\nresult = " <> source <> "\n"
  in evalModule moduleSource "result"

-- | Evaluate a Nova snippet (may contain declarations)
-- | The entry point is the function to call after loading
evalSnippet :: String -> String -> Either String String
evalSnippet source entryPoint =
  let modName = "Nova.Eval.Snippet"
      -- Wrap snippet in a module header
      moduleSource = "module " <> modName <> " where\n\n" <> source <> "\n"
  in evalModule moduleSource entryPoint

-- | Evaluate a complete module source and call an entry point
evalModule :: String -> String -> Either String String
evalModule source entryPoint =
  -- Parse the source
  case parseSource source of
    Left err -> Left ("Parse error: " <> err)
    Right coreErlang ->
      -- Compile Core Erlang to BEAM and load
      case compileAndLoad coreErlang of
        Left err -> Left ("Compile error: " <> err)
        Right modAtom ->
          -- Call the entry point function
          case callFunction modAtom entryPoint of
            Left err -> Left ("Runtime error: " <> err)
            Right result -> Right result

-- | Parse Nova source to Core Erlang string
parseSource :: String -> Either String String
parseSource = parseSourceImpl

foreign import parseSourceImpl :: String -> Either String String
  = "case catch call 'Nova.Compiler.CstPipeline':'parseModuleCst'($0) of <{'Left', Err}> when 'true' -> {'Left', Err} <{'Right', Ast}> when 'true' -> {'Right', call 'Nova.Compiler.CodeGenCoreErlang':'genModule'(Ast)} <{'EXIT', {Reason, _}}> when 'true' -> {'Left', call 'erlang':'++'([101,120,99,101,112,116,105,111,110,58,32], call 'erlang':'atom_to_list'(Reason))} <Other> when 'true' -> {'Left', [117,110,107,110,111,119,110,32,101,114,114,111,114]} end"

-- | Compile Core Erlang source and load the module
compileAndLoad :: String -> Either String String
compileAndLoad = compileAndLoadImpl

foreign import compileAndLoadImpl :: String -> Either String String
  = "let <CoreBin> = call 'erlang':'list_to_binary'($0) in case call 'core_scan':'string'(call 'erlang':'binary_to_list'(CoreBin)) of <{'ok', Tokens, _}> when 'true' -> case call 'core_parse':'parse'(Tokens) of <{'ok', CoreAst}> when 'true' -> case call 'compile':'forms'(CoreAst, ['from_core', 'binary']) of <{'ok', ModAtom, Beam}> when 'true' -> let <_> = call 'code':'load_binary'(ModAtom, [], Beam) in {'Right', call 'erlang':'atom_to_list'(ModAtom)} <{'error', Errs, _}> when 'true' -> {'Left', [99,111,109,112,105,108,101,32,101,114,114,111,114]} end <{'error', ParseErr}> when 'true' -> {'Left', call 'erlang':'++'([112,97,114,115,101,32,101,114,114,111,114,58,32], call 'lists':'flatten'(call 'io_lib':'format'([126,112], [ParseErr])))} end <{'error', ScanErr, _}> when 'true' -> {'Left', call 'erlang':'++'([115,99,97,110,32,101,114,114,111,114,58,32], call 'lists':'flatten'(call 'io_lib':'format'([126,112], [ScanErr])))} end"

-- | Call a function in a loaded module (arity 0)
callFunction :: String -> String -> Either String String
callFunction modName funcName = callFunctionImpl modName funcName

foreign import callFunctionImpl :: String -> String -> Either String String
  = "let <Mod> = call 'erlang':'list_to_atom'($0) in let <Func> = call 'erlang':'list_to_atom'($1) in case catch call Mod:Func() of <{'EXIT', {Reason, _}}> when 'true' -> {'Left', call 'lists':'flatten'(call 'io_lib':'format'([126,112], [Reason]))} <Result> when 'true' -> {'Right', call 'lists':'flatten'(call 'io_lib':'format'([126,112], [Result]))} end"

-- | Evaluate with a fresh context each time
eval :: String -> Either String String
eval source = evalExpr source

-- | Evaluate a snippet with multiple declarations
-- | Last declaration name is used as entry point if not specified
evalDecls :: String -> Either String String
evalDecls source =
  -- Try to find the last function name in the source
  let entryPoint = findLastDeclName source
  in evalSnippet source entryPoint

-- | Find the last declaration name in source (simple heuristic)
findLastDeclName :: String -> String
findLastDeclName source = findLastDeclNameImpl source

foreign import findLastDeclNameImpl :: String -> String
  = "let <Lines> = call 'string':'split'($0, [10], 'all') in let <Decls> = call 'lists':'filter'(fun (L) -> case call 'string':'trim'(L) of <[]> when 'true' -> 'false' <_> when 'true' -> case call 'string':'prefix'(L, [45,45]) of <'nomatch'> when 'true' -> 'true' <_> when 'true' -> 'false' end end, Lines) in case Decls of <[]> when 'true' -> [114,101,115,117,108,116] <_> when 'true' -> let <Last> = call 'lists':'last'(Decls) in let <Trimmed> = call 'string':'trim'(Last) in case call 'string':'split'(Trimmed, [32], 'leading') of <[Name|_]> when 'true' -> Name <[]> when 'true' -> [114,101,115,117,108,116] end end"

-- | Interactive eval - evaluate and return formatted result
ieval :: String -> String
ieval source =
  case eval source of
    Left err -> "Error: " <> err
    Right result -> result

-- | Compile a snippet to Core Erlang (for inspection)
compileToCore :: String -> Either String String
compileToCore source =
  let modName = "Nova.Eval.Debug"
      moduleSource = "module " <> modName <> " where\n\n" <> source <> "\n"
  in parseSource moduleSource
