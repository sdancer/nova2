module Nova.CoreErlang where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

-- | Module name type (Erlang atom)
type ModuleName = Unit  -- Opaque, actual atom at runtime

-- | Compiled binary code
type BinaryCode = Unit  -- Opaque, actual binary at runtime

-- | Scan Core Erlang source into tokens
scan :: String -> Either String (Array Unit)
scan source = scanImpl source

foreign import scanImpl :: String -> Either String (Array Unit) = "case call 'core_scan':'string'(call 'erlang':'binary_to_list'($0)) of <{'ok', Tokens, _}> when 'true' -> {'Right', Tokens} <{'error', Reason, _}> when 'true' -> {'Left', call 'erlang':'iolist_to_binary'(call 'io_lib':'format'(\"~p\", [Reason]))} end"

-- | Parse Core Erlang tokens into AST
parse :: Array Unit -> Either String Unit
parse tokens = parseImpl tokens

foreign import parseImpl :: Array Unit -> Either String Unit = "case call 'core_parse':'parse'($0) of <{'ok', Ast}> when 'true' -> {'Right', Ast} <{'error', Reason}> when 'true' -> {'Left', call 'erlang':'iolist_to_binary'(call 'io_lib':'format'(\"~p\", [Reason]))} end"

-- | Compile Core Erlang AST to binary code
compile :: Unit -> Either String { moduleName :: ModuleName, binary :: BinaryCode }
compile ast = compileImpl ast

foreign import compileImpl :: Unit -> Either String { moduleName :: ModuleName, binary :: BinaryCode } = "case call 'compile':'forms'($0, ['from_core', 'binary', 'return_errors']) of <{'ok', ModName, Binary}> when 'true' -> {'Right', ~{'moduleName'=>ModName, 'binary'=>Binary}~} <{'ok', ModName, Binary, _Warnings}> when 'true' -> {'Right', ~{'moduleName'=>ModName, 'binary'=>Binary}~} <{'error', Errors, _Warnings}> when 'true' -> {'Left', call 'erlang':'iolist_to_binary'(call 'io_lib':'format'(\"~p\", [Errors]))} <Other> when 'true' -> {'Left', call 'erlang':'iolist_to_binary'(call 'io_lib':'format'(\"Unexpected compile result: ~p\", [Other]))} end"

-- | Load binary code into the VM
loadBinary :: ModuleName -> BinaryCode -> Either String ModuleName
loadBinary modName binary = loadBinaryImpl modName binary

foreign import loadBinaryImpl :: ModuleName -> BinaryCode -> Either String ModuleName = "case call 'code':'load_binary'($0, \"dynamic\", $1) of <{'module', M}> when 'true' -> {'Right', M} <{'error', Reason}> when 'true' -> {'Left', call 'erlang':'iolist_to_binary'(call 'io_lib':'format'(\"~p\", [Reason]))} end"

-- | Compile and load Core Erlang source in one step
compileAndLoad :: String -> Either String ModuleName
compileAndLoad source = case scan source of
  Left err -> Left err
  Right tokens -> case parse tokens of
    Left err -> Left err
    Right ast -> case compile ast of
      Left err -> Left err
      Right result -> loadBinary result.moduleName result.binary

-- | Call a function in a loaded module
call :: forall a. ModuleName -> String -> Array a -> a
call modName funcName args = callImpl modName funcName args

foreign import callImpl :: forall a. ModuleName -> String -> Array a -> a = "call 'erlang':'apply'($0, call 'erlang':'binary_to_atom'($1, 'utf8'), $2)"

-- | Call a function with arity check
callSafe :: forall a. ModuleName -> String -> Array a -> Either String a
callSafe modName funcName args = callSafeImpl modName funcName args

foreign import callSafeImpl :: forall a. ModuleName -> String -> Array a -> Either String a = "case catch call 'erlang':'apply'($0, call 'erlang':'binary_to_atom'($1, 'utf8'), $2) of <{'EXIT', Reason}> when 'true' -> {'Left', call 'erlang':'iolist_to_binary'(call 'io_lib':'format'(\"~p\", [Reason]))} <Result> when 'true' -> {'Right', Result} end"

-- | Check if module is loaded
isLoaded :: ModuleName -> Boolean
isLoaded modName = isLoadedImpl modName

foreign import isLoadedImpl :: ModuleName -> Boolean = "case call 'code':'is_loaded'($0) of <{'file', _}> when 'true' -> 'true' <_> when 'true' -> 'false' end"

-- | Get module name as string
moduleToString :: ModuleName -> String
moduleToString modName = moduleToStringImpl modName

foreign import moduleToStringImpl :: ModuleName -> String = "call 'erlang':'atom_to_binary'($0, 'utf8')"

-- | Convert string to module name
stringToModule :: String -> ModuleName
stringToModule name = stringToModuleImpl name

foreign import stringToModuleImpl :: String -> ModuleName = "call 'erlang':'binary_to_atom'($0, 'utf8')"
