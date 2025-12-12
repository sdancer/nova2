module Nova.CLI where

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array as Array
import System.Args (ParsedArgs, parseArgs, getArgs)
import System.File (readFile, writeFile, mkdirP, joinPath, basenameNoExt)
import Effect.Console (log, logShow)

-- | Main entry point for CLI
main :: Unit -> Unit
main _ = case parseArgs getArgs of
  Left err ->
    let _ = logStderr ("Error: " <> err)
    in exit 1
  Right args ->
    if args.help
      then printHelp
      else if args.version
        then printVersion
        else case args.command of
          "compile" -> compileFiles args
          "mcp" -> startMcp args
          _ -> printHelp

-- | Print help message
printHelp :: Unit
printHelp =
  let _ = log "Nova Lang Compiler v0.1.0"
      _ = log ""
      _ = log "Usage: nova [command] [options] <files...>"
      _ = log ""
      _ = log "Commands:"
      _ = log "  mcp [options]       Start MCP (Model Context Protocol) server over TCP"
      _ = log ""
      _ = log "Compile Options:"
      _ = log "  -o, --output DIR    Output directory (default: current directory)"
      _ = log "  -d, --dep FILE      Add a dependency file (can be used multiple times)"
      _ = log ""
      _ = log "MCP Options:"
      _ = log "  -p, --port PORT     TCP port to listen on (default: 9999)"
      _ = log "  -s, --shared        Share namespaces across all connections"
      _ = log ""
      _ = log "General Options:"
      _ = log "  -h, --help          Show this help message"
      _ = log "  -v, --version       Show version"
      _ = log ""
      _ = log "Examples:"
      _ = log "  nova src/MyModule.purs"
      _ = log "  nova -o lib/ src/*.purs"
      _ = log "  nova mcp --port 8080"
  in unit

-- | Print version
printVersion :: Unit
printVersion =
  let _ = log "Nova Lang v0.1.0 (self-hosted on BEAM)"
  in unit

-- | Compile files
compileFiles :: ParsedArgs -> Unit
compileFiles args =
  let _ = case mkdirP args.outputDir of
            Left err -> logStderr ("Warning: " <> err)
            Right _ -> unit
      -- Load dependencies
      depSources = Array.mapMaybe loadDep args.deps
      -- Compile each file
      results = Array.map (compileFile args.outputDir depSources) args.files
      errors = Array.filter (\r -> r == false) results
      errorCount = Array.length errors
  in if errorCount > 0
       then let _ = logStderr ("\n" <> show errorCount <> " file(s) failed to compile")
            in exit 1
       else let _ = log ("\nCompiled " <> show (Array.length args.files) <> " file(s) successfully")
            in unit

-- | Load a dependency file
loadDep :: String -> Maybe String
loadDep path = case readFile path of
  Left err ->
    let _ = logStderr ("Warning: Could not read dependency " <> path)
    in Nothing
  Right content -> Just content

-- | Compile a single file
compileFile :: String -> Array String -> String -> Boolean
compileFile outputDir _depSources path =
  let _ = log ("Compiling " <> path <> "...")
  in case readFile path of
    Left err ->
      let _ = logStderr ("  Error: " <> err)
      in false
    Right source ->
      -- For now, just output the source as-is (placeholder for actual compilation)
      -- TODO: Call Nova.Compiler.CodeGenCoreErlang
      let basename = basenameNoExt path ".purs"
          outputPath = joinPath outputDir (basename <> ".core")
      in case writeFile outputPath source of
        Left err ->
          let _ = logStderr ("  Error writing output: " <> err)
          in false
        Right _ ->
          let _ = log ("  -> " <> outputPath)
          in true

-- | Start MCP server
startMcp :: ParsedArgs -> Unit
startMcp args =
  let _ = log ("Starting MCP server on port " <> show args.port <> "...")
      -- TODO: Implement actual MCP server start
      _ = log "MCP server not yet implemented in PureScript"
  in unit

-- | Log to stderr
logStderr :: String -> Unit
logStderr msg = logStderrImpl msg

foreign import logStderrImpl :: String -> Unit = "do call 'io':'put_chars'('standard_error', [$0, 10]) 'unit'"

-- | Exit with code
exit :: forall a. Int -> a
exit code = exitImpl code

foreign import exitImpl :: forall a. Int -> a = "call 'erlang':'halt'($0)"

-- | Show for Int
show :: Int -> String
show n = showIntImpl n

foreign import showIntImpl :: Int -> String = "call 'erlang':'integer_to_binary'($0)"

-- | Unit value
unit :: Unit
unit = unitImpl

foreign import unitImpl :: Unit = "'unit'"
