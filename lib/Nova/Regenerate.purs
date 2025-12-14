-- | BEAM-native regenerate script
-- |
-- | This is the BEAM equivalent of `node scripts/regenerate-purs.js`.
-- | It compiles all lib/ and src/ .purs files, generates Core Erlang output,
-- | and writes to both output/ and nova_lang/priv/core/.
-- |
-- | Usage from Erlang:   'Nova.Regenerate':main()
-- | Usage from Elixir:   :"Nova.Regenerate".main()
-- |
-- | NOTE: Uses dummy variable names instead of _ to work around type checker bug
module Nova.Regenerate where

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array as Array
import Data.Array (fromFoldable)
import Data.List (List)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Set as Set
import Data.String as String
import Nova.Compiler.CstPipeline (parseModuleCst)
import Nova.Compiler.CodeGenCoreErlang (genModule)
import Nova.Compiler.TypeChecker (checkModule, extractExports, addValuesToExports)
import Nova.Compiler.Types (emptyRegistry, emptyEnv, preludeExports, registerModule, ModuleRegistry, ModuleExports)

-- ============================================================================
-- Configuration
-- ============================================================================

type Config =
  { srcBase :: String
  , libBase :: String
  , outputDir :: String
  , targetDir :: String
  }

defaultConfig :: Config
defaultConfig =
  { srcBase: "src/"
  , libBase: "lib/"
  , outputDir: "output/"
  , targetDir: "nova_lang/priv/core/"
  }

-- ============================================================================
-- Main Entry Point
-- ============================================================================

-- | Main entry point - call from Erlang as 'Nova.Regenerate':main('unit')
main :: Unit -> Unit
main u0 = runRegenerate defaultConfig

-- | Run regeneration with timing
runRegenerate :: Config -> Unit
runRegenerate config =
  let u1 = log "=== Nova Compiler Regeneration (BEAM) ==="
      u2 = log ""
      totalStart = getMonotonicMs

      -- Find all source files
      u3 = log "Finding source files..."
      libFiles = findFiles config.libBase ".purs"
      srcFiles = findFiles config.srcBase ".purs"
      allFiles = Array.concat [libFiles, srcFiles]
      u4 = log ("Found " <> showInt (Array.length libFiles) <> " lib files, " <> showInt (Array.length srcFiles) <> " src files")
      u5 = log ""

      -- Build dependency graph
      u6 = log "Building dependency graph..."
      graphStart = getMonotonicMs
      deps = buildDependencyGraph config allFiles
      graphTime = getMonotonicMs - graphStart
      u7 = log ("Dependency graph built in " <> formatTime graphTime)
      u8 = log ""

      -- Topological sort
      u9 = log "Topological sort..."
      sortStart = getMonotonicMs
      sorted = topoSort allFiles deps
      sortTime = getMonotonicMs - sortStart
      u10 = log ("Sorted " <> showInt (Array.length sorted) <> " modules in " <> formatTime sortTime)
      u11 = log ""

      -- Compile modules
      u12 = log "=== Compiling Modules ==="
      u13 = log ""
      result = compileModules config sorted

      totalTime = getMonotonicMs - totalStart
      u14 = log ""
      u15 = log "=== Summary ==="
      u16 = log ("Compiled: " <> showInt result.compiled)
      u17 = log ("Errors: " <> showInt result.errors)
      u18 = log ("Total time: " <> formatTime totalTime)
  in unit

-- ============================================================================
-- Foreign Imports (BEAM-specific)
-- ============================================================================

-- | Log to stdout with newline
log :: String -> Unit
log msg = logImpl msg

foreign import logImpl :: String -> Unit = "do call 'io':'put_chars'([$0, 10]) 'unit'"

-- | Log without newline
logNoNewline :: String -> Unit
logNoNewline msg = logNoNewlineImpl msg

foreign import logNoNewlineImpl :: String -> Unit = "do call 'io':'put_chars'($0) 'unit'"

-- | Unit value
unit :: Unit
unit = unitImpl

foreign import unitImpl :: Unit = "'unit'"

-- | Read file contents
readFile :: String -> Maybe String
readFile path = readFileImpl path

foreign import readFileImpl :: String -> Maybe String = "case call 'file':'read_file'($0) of <{'ok', Content}> when 'true' -> {'Just', Content} <{'error', _Reason}> when 'true' -> 'Nothing' end"

-- | Write file contents
writeFile :: String -> String -> Unit
writeFile path content = writeFileImpl path content

foreign import writeFileImpl :: String -> String -> Unit = "do call 'file':'write_file'($0, $1) 'unit'"

-- | Check if file exists
fileExists :: String -> Boolean
fileExists path = fileExistsImpl path

foreign import fileExistsImpl :: String -> Boolean = "call 'filelib':'is_regular'($0)"

-- | Find all files with extension recursively
findFiles :: String -> String -> Array String
findFiles dir ext = findFilesImpl dir ext

foreign import findFilesImpl :: String -> String -> Array String = "letrec 'doFind'/2 = fun (Dir, Ext) -> let <DirList> = call 'erlang':'binary_to_list'(Dir) in case call 'file':'list_dir'(DirList) of <{'ok', Entries}> when 'true' -> call 'lists':'foldl'(fun (Entry, Acc) -> let <Full> = call 'filename':'join'(DirList, Entry) in let <FullBin> = call 'erlang':'list_to_binary'(Full) in case call 'filelib':'is_dir'(Full) of <'true'> when 'true' -> call 'lists':'append'(Acc, apply 'doFind'/2 (FullBin, Ext)) <'false'> when 'true' -> case call 'lists':'suffix'(call 'erlang':'binary_to_list'(Ext), Entry) of <'true'> when 'true' -> [FullBin|Acc] <'false'> when 'true' -> Acc end end, [], Entries) <{'error', _Reason}> when 'true' -> [] end in apply 'doFind'/2 ($0, $1)"

-- | Ensure directory exists
ensureDir :: String -> Unit
ensureDir dir = ensureDirImpl dir

foreign import ensureDirImpl :: String -> Unit = "let <_Ignore> = call 'filelib':'ensure_dir'(call 'erlang':'iolist_to_binary'([$0, [47]])) in 'unit'"

-- | Get directory name
dirname :: String -> String
dirname path = dirnameImpl path

foreign import dirnameImpl :: String -> String = "call 'filename':'dirname'($0)"

-- | Get monotonic time in milliseconds
getMonotonicMs :: Int
getMonotonicMs = getMonotonicMsImpl

foreign import getMonotonicMsImpl :: Int = "call 'erlang':'div'(call 'erlang':'monotonic_time'('nanosecond'), 1000000)"

-- | Format time for display
formatTime :: Int -> String
formatTime ms = formatTimeImpl ms

foreign import formatTimeImpl :: Int -> String = "case call 'erlang':'>='($0, 1000) of <'true'> when 'true' -> call 'erlang':'iolist_to_binary'([call 'erlang':'float_to_binary'(call 'erlang':'/'($0, 1000), [{'decimals', 2}]), call 'erlang':'list_to_binary'([115])]) <'false'> when 'true' -> call 'erlang':'iolist_to_binary'([call 'erlang':'integer_to_binary'($0), call 'erlang':'list_to_binary'([109, 115])]) end"

-- | Show Int as String
showInt :: Int -> String
showInt n = showIntImpl n

foreign import showIntImpl :: Int -> String = "call 'erlang':'integer_to_binary'($0)"

-- | Show any value as String (for error messages)
showAny :: forall a. a -> String
showAny x = showAnyImpl x

foreign import showAnyImpl :: forall a. a -> String = "call 'erlang':'iolist_to_binary'(call 'io_lib':'format'(\"~p\", [$0]))"

-- ============================================================================
-- Import Extraction
-- ============================================================================

-- | Extract import statements from PureScript source
extractImports :: String -> Array String
extractImports source =
  let lines = String.split (String.Pattern "\n") source
  in Array.mapMaybe extractImportFromLine lines

-- | Extract module name from an import line
extractImportFromLine :: String -> Maybe String
extractImportFromLine line =
  case String.indexOf (String.Pattern "import ") line of
    Nothing -> Nothing
    Just idx ->
      let rest = String.drop (idx + 7) line
          modName = takeModuleName rest
      in if String.null modName then Nothing else Just modName

-- | Take module name characters (A-Z, a-z, 0-9, .)
takeModuleName :: String -> String
takeModuleName s =
  let codePoints = String.toCodePointArray s
      validCount = countValidChars codePoints 0
  in String.take validCount s
  where
    countValidChars :: Array Int -> Int -> Int
    countValidChars cps idx =
      case Array.index cps idx of
        Nothing -> idx
        Just cp ->
          if isModuleChar cp
          then countValidChars cps (idx + 1)
          else idx

    isModuleChar :: Int -> Boolean
    isModuleChar cp =
      (cp >= 65 && cp <= 90) ||   -- A-Z
      (cp >= 97 && cp <= 122) ||  -- a-z
      (cp >= 48 && cp <= 57) ||   -- 0-9
      cp == 46                     -- .

-- | Convert module name to file path
-- | Special case: "Prelude" maps to Nova.Prelude
moduleToPath :: Config -> String -> Maybe String
moduleToPath config modName =
  -- Special case: "Prelude" maps to Nova.Prelude
  if modName == "Prelude"
  then Just (config.libBase <> "Nova/Prelude.purs")
  else
    let relPath = String.replaceAll (String.Pattern ".") (String.Replacement "/") modName <> ".purs"
        libPath = config.libBase <> relPath
        srcPath = config.srcBase <> relPath
    in if fileExists libPath then Just libPath
       else if fileExists srcPath then Just srcPath
       else Nothing

-- ============================================================================
-- Dependency Graph
-- ============================================================================

-- | Build dependency graph from files
buildDependencyGraph :: Config -> Array String -> Map.Map String (Array String)
buildDependencyGraph config files =
  Array.foldl (\acc filePath ->
    case readFile filePath of
      Nothing -> acc
      Just source ->
        let imports = extractImports source
            depPaths = Array.mapMaybe (moduleToPath config) imports
            filtered = Array.filter (\p -> p /= filePath) depPaths
        in Map.insert filePath filtered acc
  ) Map.empty files

-- | Topological sort using DFS
-- Note: We visit dependencies first, then add self to sorted.
-- This naturally produces the correct order (dependencies before dependents).
topoSort :: Array String -> Map.Map String (Array String) -> Array String
topoSort files deps =
  let result = Array.foldl visitFromRoot { visited: Set.empty, sorted: [] } files
  in result.sorted
  where
    visitFromRoot :: { visited :: Set.Set String, sorted :: Array String } -> String -> { visited :: Set.Set String, sorted :: Array String }
    visitFromRoot state path =
      if Set.member path state.visited
      then state
      else visit path state

    visit :: String -> { visited :: Set.Set String, sorted :: Array String } -> { visited :: Set.Set String, sorted :: Array String }
    visit path state =
      if Set.member path state.visited
      then state
      else
        let pathDeps = case Map.lookup path deps of
              Just d -> d
              Nothing -> []
            validDeps = Array.filter (\d -> Map.member d deps) pathDeps
            state1 = state { visited = Set.insert path state.visited }
            state2 = Array.foldl (\s dep -> visit dep s) state1 validDeps
        in state2 { sorted = Array.snoc state2.sorted path }

-- ============================================================================
-- Module Compilation
-- ============================================================================

-- | Get module name from file path
getModuleName :: Config -> String -> String
getModuleName config filePath =
  let baseLen = if String.take (String.length config.libBase) filePath == config.libBase
                then String.length config.libBase
                else if String.take (String.length config.srcBase) filePath == config.srcBase
                then String.length config.srcBase
                else 0
      relative = String.drop baseLen filePath
      withoutExt = String.take (String.length relative - 5) relative
  in String.replaceAll (String.Pattern "/") (String.Replacement ".") withoutExt

-- | Compilation result
type CompileResult =
  { compiled :: Int
  , errors :: Int
  , registry :: ModuleRegistry
  }

-- | Compile all modules in order
compileModules :: Config -> Array String -> CompileResult
compileModules config sorted =
  let initialRegistry = registerModule emptyRegistry "Prelude" preludeExports
  in Array.foldl (compileModule config) { compiled: 0, errors: 0, registry: initialRegistry } sorted

-- | Compile a single module
compileModule :: Config -> CompileResult -> String -> CompileResult
compileModule config result filePath =
  let modName = getModuleName config filePath
  in case readFile filePath of
    Nothing ->
      let u0 = log (modName <> "... READ ERROR")
      in result { errors = result.errors + 1 }
    Just source ->
      let bytes = String.length source
          u0 = logNoNewline (modName <> " (" <> showInt bytes <> " bytes)... ")
          modStart = getMonotonicMs
          parseStart = getMonotonicMs
      in case parseModuleCst source of
        Left err ->
          let u1 = log ("PARSE ERROR: " <> err)
          in result { errors = result.errors + 1 }
        Right mod ->
          let parseTime = getMonotonicMs - parseStart
              tcStart = getMonotonicMs
              declsArray = fromFoldable mod.declarations
          in case checkModule result.registry emptyEnv declsArray of
            Left err ->
              let u1 = log ("TYPE ERROR (parse: " <> formatTime parseTime <> "): " <> showAny err)
              in result { errors = result.errors + 1 }
            Right env ->
              let tcTime = getMonotonicMs - tcStart
                  codegenStart = getMonotonicMs
                  codegenResult = genModule mod
                  codegenTime = getMonotonicMs - codegenStart
              in case codegenResult of
                Left codegenErr ->
                  let u1 = log ("CODEGEN ERROR (parse: " <> formatTime parseTime <> ", tc: " <> formatTime tcTime <> "): " <> codegenErr)
                  in result { errors = result.errors + 1 }
                Right code ->
                  let modPath = String.replaceAll (String.Pattern ".") (String.Replacement "/") modName
                      outputFile = config.outputDir <> modPath <> ".core"
                      targetFile = config.targetDir <> modPath <> ".core"

                      u1 = ensureDir (dirname outputFile)
                      u2 = ensureDir (dirname targetFile)
                      u3 = writeFile outputFile code
                      u4 = writeFile targetFile code

                      exports = extractExports declsArray
                      exportsWithValues = addValuesToExports exports env declsArray
                      -- When compiling Nova.Prelude, register as "Prelude" with primitive operators merged
                      newRegistry = if modName == "Nova.Prelude"
                                    then let preludeWithPrimitives = mergeWithPrimitiveOps exportsWithValues
                                             reg1 = registerModule result.registry "Prelude" preludeWithPrimitives
                                         in registerModule reg1 modName exportsWithValues
                                    else registerModule result.registry modName exportsWithValues

                      totalTime = getMonotonicMs - modStart
                      lines = countLines code
                      u5 = log ("OK [parse: " <> formatTime parseTime <> ", tc: " <> formatTime tcTime <> ", codegen: " <> formatTime codegenTime <> ", total: " <> formatTime totalTime <> "] (" <> showInt lines <> " lines)")
                  in result { compiled = result.compiled + 1, registry = newRegistry }

-- | Merge dynamically extracted exports with primitive operators from preludeExports
-- | This ensures primitive operators like *>, <$>, etc. remain available
mergeWithPrimitiveOps :: ModuleExports -> ModuleExports
mergeWithPrimitiveOps exports =
  exports { values = Map.union exports.values preludeExports.values }

-- | Count lines in a string
countLines :: String -> Int
countLines s = Array.length (String.split (String.Pattern "\n") s)
