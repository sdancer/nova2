module System.Args where

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array as Array

-- | Get command line arguments
getArgs :: Array String
getArgs = getArgsImpl

foreign import getArgsImpl :: Array String = "call 'lists':'map'(fun (A) -> call 'erlang':'list_to_binary'(A), call 'init':'get_plain_arguments'())"

-- | Get script name
getProgName :: String
getProgName = getProgNameImpl

foreign import getProgNameImpl :: String = "case call 'init':'get_argument'('progname') of <{'ok', [[Name]]}> when 'true' -> call 'erlang':'list_to_binary'(Name) <_> when 'true' -> #{#<110>(8,1,'integer',['unsigned'|['big']]),#<111>(8,1,'integer',['unsigned'|['big']]),#<118>(8,1,'integer',['unsigned'|['big']]),#<97>(8,1,'integer',['unsigned'|['big']])}# end"

-- | Parsed command line options
type ParsedArgs =
  { command :: String
  , files :: Array String
  , outputDir :: String
  , deps :: Array String
  , port :: Int
  , shared :: Boolean
  , help :: Boolean
  , version :: Boolean
  }

-- | Default parsed args
defaultArgs :: ParsedArgs
defaultArgs =
  { command: "compile"
  , files: []
  , outputDir: "."
  , deps: []
  , port: 9999
  , shared: false
  , help: false
  , version: false
  }

-- | Parse command line arguments
parseArgs :: Array String -> Either String ParsedArgs
parseArgs args = parseArgsLoop args defaultArgs

parseArgsLoop :: Array String -> ParsedArgs -> Either String ParsedArgs
parseArgsLoop args acc = case Array.uncons args of
  Nothing ->
    if Array.null acc.files && not acc.help && not acc.version
      then Right (acc { help = true })
      else Right acc
  Just { head: arg, tail: rest } ->
    case arg of
      "mcp" -> parseArgsLoop rest (acc { command = "mcp" })
      "--help" -> Right (acc { help = true })
      "-h" -> Right (acc { help = true })
      "--version" -> Right (acc { version = true })
      "-v" -> Right (acc { version = true })
      "--output" -> case Array.head rest of
        Just dir -> parseArgsLoop (Array.drop 1 rest) (acc { outputDir = dir })
        Nothing -> Left "Missing argument for --output"
      "-o" -> case Array.head rest of
        Just dir -> parseArgsLoop (Array.drop 1 rest) (acc { outputDir = dir })
        Nothing -> Left "Missing argument for -o"
      "--dep" -> case Array.head rest of
        Just dep -> parseArgsLoop (Array.drop 1 rest) (acc { deps = Array.snoc acc.deps dep })
        Nothing -> Left "Missing argument for --dep"
      "-d" -> case Array.head rest of
        Just dep -> parseArgsLoop (Array.drop 1 rest) (acc { deps = Array.snoc acc.deps dep })
        Nothing -> Left "Missing argument for -d"
      "--port" -> case Array.head rest of
        Just portStr -> case parseInt portStr of
          Just port -> parseArgsLoop (Array.drop 1 rest) (acc { port = port })
          Nothing -> Left ("Invalid port number: " <> portStr)
        Nothing -> Left "Missing argument for --port"
      "-p" -> case Array.head rest of
        Just portStr -> case parseInt portStr of
          Just port -> parseArgsLoop (Array.drop 1 rest) (acc { port = port })
          Nothing -> Left ("Invalid port number: " <> portStr)
        Nothing -> Left "Missing argument for -p"
      "--shared" -> parseArgsLoop rest (acc { shared = true })
      "-s" -> parseArgsLoop rest (acc { shared = true })
      _ ->
        if startsWith "-" arg
          then Left ("Unknown option: " <> arg)
          else parseArgsLoop rest (acc { files = Array.snoc acc.files arg })

-- | Parse an integer from string
parseInt :: String -> Maybe Int
parseInt s = parseIntImpl s

foreign import parseIntImpl :: String -> Maybe Int = "case catch call 'erlang':'binary_to_integer'($0) of <N> when call 'erlang':'is_integer'(N) -> {'Just', N} <_> when 'true' -> 'Nothing' end"

-- | Check if string starts with prefix
startsWith :: String -> String -> Boolean
startsWith prefix str = startsWithImpl prefix str

foreign import startsWithImpl :: String -> String -> Boolean = "let <PrefixLen> = call 'erlang':'byte_size'($0) in let <StrLen> = call 'erlang':'byte_size'($1) in case call 'erlang':'>='(StrLen, PrefixLen) of <'true'> when 'true' -> call 'erlang':'=:='(call 'erlang':'binary_part'($1, 0, PrefixLen), $0) <'false'> when 'true' -> 'false' end"
