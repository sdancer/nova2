-- | Regenerate module: Compiles Nova compiler modules to Elixir
-- |
-- | This module contains the core logic for regenerating the compiler.
-- | Filesystem operations are passed in as delegates, making this module
-- | compatible with both Node.js and Elixir backends without FFI.
-- |
-- | Note: This module requires complete library module implementations
-- | (Data.String, Data.Array, etc.) to work correctly. The lib/ stubs
-- | are incomplete, so full regeneration currently still requires the
-- | original regenerate.js which uses PureScript-compiled modules.
module Nova.Compiler.Regenerate where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))

import Nova.Compiler.Ast (Module)
import Nova.Compiler.CstPipeline as CstPipeline
import Nova.Compiler.CodeGenCoreErlang as CodeGen
import Nova.Compiler.TypeChecker as TypeChecker
import Nova.Compiler.Types as Types

-- ============================================================================
-- Filesystem Delegates
-- ============================================================================

-- | Filesystem operations required by the regenerate process
-- | These are passed in by the caller (JS or Elixir runtime)
type FileSystem =
  { readFile :: String -> Maybe String           -- Read file contents, Nothing if not found
  , writeFile :: String -> String -> Unit        -- Write file contents
  , fileExists :: String -> Boolean              -- Check if file exists
  , listFiles :: String -> String -> Array String -- List files in dir matching extension
  }

-- ============================================================================
-- Configuration
-- ============================================================================

-- | Configuration for regeneration
type RegenerateConfig =
  { srcBase :: String      -- Base path for compiler sources (e.g., "./src/Nova/Compiler/")
  , libBase :: String      -- Base path for library sources (e.g., "./lib/")
  , outputDir :: String    -- Output directory (e.g., "./output/")
  , targetDir :: String    -- Target directory for Elixir files (e.g., "./nova_lang/lib/nova/compiler/")
  }

defaultConfig :: RegenerateConfig
defaultConfig =
  { srcBase: "./src/"
  , libBase: "./lib/"
  , outputDir: "./output/"
  , targetDir: "./nova_lang/priv/core/"
  }

-- ============================================================================
-- Types
-- ============================================================================

-- | Result of parsing and type-checking a module
type ModuleResult =
  { mod :: Module
  , env :: Types.Env
  , exports :: Types.ModuleExports
  }

-- | Compilation log entry
data LogEntry
  = LogInfo String
  | LogWarning String
  | LogError String

-- | Result of the regeneration process
type RegenerateResult =
  { success :: Boolean
  , modulesCompiled :: Int
  , logs :: Array LogEntry
  }

-- ============================================================================
-- Import Extraction
-- ============================================================================

-- | Extract import statements from PureScript source
-- | Returns array of imported module names
extractImports :: String -> Array String
extractImports source =
  let lines = String.split (String.Pattern "\n") source
      extractImport line =
        -- Match: import ModuleName or import ModuleName (...)
        case String.indexOf (String.Pattern "import ") line of
          Nothing -> Nothing
          Just idx ->
            let rest = String.drop (idx + 7) line
                -- Get first word (module name)
                parts = String.split (String.Pattern " ") rest
            in case Array.head parts of
              Nothing -> Nothing
              Just modName ->
                -- Clean up: remove parens - take until first '('
                let cleanName = case String.indexOf (String.Pattern "(") modName of
                      Nothing -> modName
                      Just parenIdx -> String.take parenIdx modName
                in if String.null cleanName then Nothing else Just cleanName
  in Array.mapMaybe extractImport lines

-- | Convert module name to file path
-- | First checks lib/, then src/ - no hardcoded prefixes
-- | e.g., "Nova.Compiler.Ast" -> "./src/Nova/Compiler/Ast.purs"
-- | e.g., "Data.List" -> "./lib/Data/List.purs"
-- | e.g., "Nova.NamespaceService" -> "./lib/Nova/NamespaceService.purs"
moduleToPath :: FileSystem -> RegenerateConfig -> String -> Maybe String
moduleToPath fs cfg modName =
  let relPath = String.replaceAll (String.Pattern ".") (String.Replacement "/") modName <> ".purs"
      libPath = cfg.libBase <> relPath
      srcPath = cfg.srcBase <> relPath
  in -- First check lib/ (library modules can be anywhere)
     if fs.fileExists libPath
     then Just libPath
     -- Then check src/ (compiler modules)
     else if fs.fileExists srcPath
     then Just srcPath
     else Nothing

-- | Discover dependencies for a module by parsing its imports
discoverDependencies :: FileSystem -> RegenerateConfig -> String -> Boolean -> Array String
discoverDependencies fs cfg path _isLibModule =
  case fs.readFile path of
    Nothing -> []
    Just source ->
      let imports = extractImports source
          toDep modName = moduleToPath fs cfg modName
      in Array.filter (\p -> p /= path) (Array.mapMaybe toDep imports)

-- ============================================================================
-- Dependency Graph
-- ============================================================================

-- | Build dependency graph from module paths
buildDependencyGraph :: FileSystem -> RegenerateConfig -> Array String -> Boolean -> Map String (Array String)
buildDependencyGraph fs cfg paths isLibModules =
  Map.fromFoldable (map (\path -> Tuple path (discoverDependencies fs cfg path isLibModules)) paths)

-- | Topologically sort modules based on dependencies
-- | Returns modules in order such that dependencies come before dependents
topologicalSort :: Map String (Array String) -> Array String
topologicalSort graph = go (Array.fromFoldable (Map.keys graph)) Set.empty Set.empty []
  where
    go :: Array String -> Set String -> Set String -> Array String -> Array String
    go remaining visited visiting acc =
      case Array.uncons remaining of
        Nothing -> acc  -- Don't reverse - deps are already in correct order
        Just { head: path, tail: rest } ->
          if Set.member path visited
          then go rest visited visiting acc
          else
            let result = visit path visited visiting acc
            in go rest result.visited result.visiting result.acc

    visit :: String -> Set String -> Set String -> Array String -> { visited :: Set String, visiting :: Set String, acc :: Array String }
    visit path visited visiting acc =
      if Set.member path visited
      then { visited, visiting, acc }
      else if Set.member path visiting
      then
        -- Circular dependency - skip with warning
        { visited, visiting, acc }
      else
        let visiting' = Set.insert path visiting
            deps = case Map.lookup path graph of
              Just d -> d
              Nothing -> []
            -- Only visit deps that are in our graph
            validDeps = Array.filter (\d -> Map.member d graph) deps
            result = foldl (\r d -> visit d r.visited r.visiting r.acc) { visited, visiting: visiting', acc } validDeps
            visited' = Set.insert path result.visited
            visiting'' = Set.delete path result.visiting
        in { visited: visited', visiting: visiting'', acc: Array.snoc result.acc path }

-- ============================================================================
-- Module Compilation
-- ============================================================================

-- | Parse and type-check a module
parseAndCheckModule :: Types.ModuleRegistry -> String -> Either String ModuleResult
parseAndCheckModule registry source =
  case CstPipeline.parseModuleCst source of
    Left err -> Left ("Parse error: " <> err)
    Right mod ->
      -- Type check with registry
      case TypeChecker.checkModule registry Types.emptyEnv (Array.fromFoldable mod.declarations) of
        Left err -> Left ("Type error: " <> show err)
        Right env ->
          let modDeclsArray = Array.fromFoldable mod.declarations
              exports = TypeChecker.extractExports modDeclsArray
              exportsWithValues = TypeChecker.addValuesToExports exports env modDeclsArray
          in Right { mod, env, exports: exportsWithValues }

-- | Compile a single module to Elixir code
compileModule :: Types.ModuleRegistry -> String -> Either String String
compileModule registry source =
  case parseAndCheckModule registry source of
    Left err -> Left err
    Right result ->
      let code = CodeGen.genModule result.mod
      in Right code

-- ============================================================================
-- Utility Functions
-- ============================================================================

-- | Get module name from file path
getModuleName :: RegenerateConfig -> String -> String
getModuleName cfg path =
  -- Library modules
  if String.indexOf (String.Pattern cfg.libBase) path == Just 0
  then
    let relative = String.drop (String.length cfg.libBase) path
        withoutExt = String.take (String.length relative - 5) relative  -- Remove ".purs"
    in String.replaceAll (String.Pattern "/") (String.Replacement ".") withoutExt
  -- Source modules
  else if String.indexOf (String.Pattern cfg.srcBase) path == Just 0
  then
    let relative = String.drop (String.length cfg.srcBase) path
        withoutExt = String.take (String.length relative - 5) relative  -- Remove ".purs"
    in String.replaceAll (String.Pattern "/") (String.Replacement ".") withoutExt
  else
    let match = getFileBaseName path
    in match

-- | Get base filename without extension
getFileBaseName :: String -> String
getFileBaseName path =
  let parts = String.split (String.Pattern "/") path
  in case Array.last parts of
    Nothing -> path
    Just filename ->
      String.take (String.length filename - 5) filename  -- Remove ".purs"

-- | Get short display name for a path
getShortName :: String -> String
getShortName = getFileBaseName

-- ============================================================================
-- Main Regeneration Logic
-- ============================================================================

-- | Compile all library modules, building up a registry
compileLibraryModules :: FileSystem -> RegenerateConfig -> Array String -> Tuple Types.ModuleRegistry (Array LogEntry)
compileLibraryModules fs cfg sortedPaths =
  let initial = Tuple (Types.registerModule Types.emptyRegistry "Prelude" Types.preludeExports) []
  in foldl (compileLibModule fs cfg) initial sortedPaths
  where
    compileLibModule :: FileSystem -> RegenerateConfig -> Tuple Types.ModuleRegistry (Array LogEntry) -> String -> Tuple Types.ModuleRegistry (Array LogEntry)
    compileLibModule fs' cfg' (Tuple registry logs) path =
      case fs'.readFile path of
        Nothing ->
          Tuple registry (Array.snoc logs (LogError ("Cannot read: " <> path)))
        Just source ->
          let modName = getModuleName cfg' path
          in case parseAndCheckModule registry source of
            Left err ->
              Tuple registry (Array.snoc logs (LogError (modName <> ": " <> err)))
            Right result ->
              let registry' = Types.registerModule registry modName result.exports
                  -- Generate and write .core file for library module
                  code = CodeGen.genModule result.mod
                  -- Convert "Data.Maybe" to "Data/Maybe.core"
                  modPath = String.replaceAll (String.Pattern ".") (String.Replacement "/") modName
                  outputFile = cfg'.outputDir <> modPath <> ".core"
                  targetFile = cfg'.targetDir <> modPath <> ".core"
                  _written1 = fs'.writeFile outputFile code
                  _written2 = fs'.writeFile targetFile code
              in Tuple registry' (Array.snoc logs (LogInfo ("Compiled library: " <> modName)))

-- | Compile all source modules (src/**), generating Core Erlang output
compileSourceModules :: FileSystem -> RegenerateConfig -> Types.ModuleRegistry -> Array String -> Tuple Int (Array LogEntry)
compileSourceModules fs cfg libRegistry sortedPaths =
  let initial = { registry: libRegistry, count: 0, logs: [] }
      result = foldl (compileSrcModule fs cfg) initial sortedPaths
  in Tuple result.count result.logs
  where
    compileSrcModule fs' cfg' acc path =
      case fs'.readFile path of
        Nothing ->
          acc { logs = Array.snoc acc.logs (LogError ("Cannot read: " <> path)) }
        Just source ->
          let fullModName = getModuleName cfg' path
          in case parseAndCheckModule acc.registry source of
            Left err ->
              acc { logs = Array.snoc acc.logs (LogError (fullModName <> ": " <> err)) }
            Right result ->
              let code = CodeGen.genModule result.mod
                  registry' = Types.registerModule acc.registry fullModName result.exports
                  -- Convert "Nova.Compiler.Ast" to "Nova/Compiler/Ast.core"
                  modPath = String.replaceAll (String.Pattern ".") (String.Replacement "/") fullModName
                  outputFile = cfg'.outputDir <> modPath <> ".core"
                  targetFile = cfg'.targetDir <> modPath <> ".core"
                  _written1 = fs'.writeFile outputFile code
                  _written2 = fs'.writeFile targetFile code
                  lineCount = Array.length (String.split (String.Pattern "\n") code)
                  logMsg = "Compiled " <> fullModName <> " (" <> show lineCount <> " lines)"
              in { registry: registry'
                 , count: acc.count + 1
                 , logs: Array.snoc acc.logs (LogInfo logMsg)
                 }

-- | Main entry point: regenerate all compiler modules
-- | Combines lib/ and src/ files into one dependency graph and sorts topologically
regenerate :: FileSystem -> RegenerateConfig -> RegenerateResult
regenerate fs cfg =
  let -- Find all source files (lib/** and src/**)
      libFiles = fs.listFiles cfg.libBase ".purs"
      srcFiles = fs.listFiles cfg.srcBase ".purs"

      -- Combine all files into one list
      allFiles = libFiles <> srcFiles

      -- Build ONE combined dependency graph for all files
      combinedDeps = buildDependencyGraph fs cfg allFiles false

      -- Sort topologically - dependencies come before dependents
      sortedModules = topologicalSort combinedDeps

      -- Compile all modules in order, building registry as we go
      Tuple count logs = compileAllModules fs cfg sortedModules
  in { success: true
     , modulesCompiled: count
     , logs: logs
     }

-- | Compile all modules (lib and src combined), building up registry incrementally
compileAllModules :: FileSystem -> RegenerateConfig -> Array String -> Tuple Int (Array LogEntry)
compileAllModules fs cfg sortedPaths =
  let initial = { registry: Types.registerModule Types.emptyRegistry "Prelude" Types.preludeExports, count: 0, logs: [] }
      result = foldl (compileOneModule fs cfg) initial sortedPaths
  in Tuple result.count result.logs
  where
    compileOneModule :: FileSystem -> RegenerateConfig -> { registry :: Types.ModuleRegistry, count :: Int, logs :: Array LogEntry } -> String -> { registry :: Types.ModuleRegistry, count :: Int, logs :: Array LogEntry }
    compileOneModule fs' cfg' acc path =
      case fs'.readFile path of
        Nothing ->
          acc { logs = Array.snoc acc.logs (LogError ("Cannot read: " <> path)) }
        Just source ->
          let fullModName = getModuleName cfg' path
          in case parseAndCheckModule acc.registry source of
            Left err ->
              acc { logs = Array.snoc acc.logs (LogError (fullModName <> ": " <> err)) }
            Right result ->
              let code = CodeGen.genModule result.mod
                  registry' = Types.registerModule acc.registry fullModName result.exports
                  -- Convert "Nova.Compiler.Ast" to "Nova/Compiler/Ast.core"
                  modPath = String.replaceAll (String.Pattern ".") (String.Replacement "/") fullModName
                  outputFile = cfg'.outputDir <> modPath <> ".core"
                  targetFile = cfg'.targetDir <> modPath <> ".core"
                  _written1 = fs'.writeFile outputFile code
                  _written2 = fs'.writeFile targetFile code
                  lineCount = Array.length (String.split (String.Pattern "\n") code)
                  logMsg = "Compiled " <> fullModName <> " (" <> show lineCount <> " lines)"
              in { registry: registry'
                 , count: acc.count + 1
                 , logs: Array.snoc acc.logs (LogInfo logMsg)
                 }

-- ============================================================================
-- Logging Helpers
-- ============================================================================

showLogEntry :: LogEntry -> String
showLogEntry (LogInfo msg) = "[INFO] " <> msg
showLogEntry (LogWarning msg) = "[WARN] " <> msg
showLogEntry (LogError msg) = "[ERROR] " <> msg

showLogs :: Array LogEntry -> String
showLogs = String.joinWith "\n" <<< map showLogEntry
