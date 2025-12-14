-- | Regenerate module: Compiles Nova compiler modules to Core Erlang
-- |
-- | This module contains the core logic for regenerating the compiler.
-- | Filesystem operations are passed in as delegates, making this module
-- | compatible with both Node.js and Elixir backends without FFI.
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
-- Filesystem Delegates
-- ============================================================================

-- | Filesystem operations required by the regenerate process
type FileSystem =
  { readFile :: String -> Maybe String
  , writeFile :: String -> String -> Unit
  , fileExists :: String -> Boolean
  , listFiles :: String -> String -> Array String
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
extractImports :: String -> Array String
extractImports source =
  let lines = String.split (String.Pattern "\n") source
      extractImport line =
        case String.indexOf (String.Pattern "import ") line of
          Nothing -> Nothing
          Just idx ->
            let rest = String.drop (idx + 7) line
                parts = String.split (String.Pattern " ") rest
            in case Array.head parts of
              Nothing -> Nothing
              Just modName ->
                let cleanName = case String.indexOf (String.Pattern "(") modName of
                      Nothing -> modName
                      Just parenIdx -> String.take parenIdx modName
                in if String.null cleanName then Nothing else Just cleanName
  in Array.mapMaybe extractImport lines

-- | Convert module name to file path
moduleToPath :: FileSystem -> Config -> String -> Maybe String
moduleToPath fs config modName =
  let relPath = String.replaceAll (String.Pattern ".") (String.Replacement "/") modName <> ".purs"
      libPath = config.libBase <> relPath
      srcPath = config.srcBase <> relPath
  in if fs.fileExists libPath then Just libPath
     else if fs.fileExists srcPath then Just srcPath
     else Nothing

-- ============================================================================
-- Dependency Graph
-- ============================================================================

-- | Build dependency graph from module paths
buildDependencyGraph :: FileSystem -> Config -> Array String -> Map String (Array String)
buildDependencyGraph fs config files =
  Map.fromFoldable (map (\path ->
    let deps = case fs.readFile path of
          Nothing -> []
          Just source ->
            let imports = extractImports source
                depPaths = Array.mapMaybe (moduleToPath fs config) imports
            in Array.filter (\p -> p /= path) depPaths
    in Tuple path deps
  ) files)

-- | Topologically sort modules based on dependencies
topologicalSort :: Map String (Array String) -> Array String
topologicalSort graph = go (Array.fromFoldable (Map.keys graph)) Set.empty Set.empty []
  where
    go :: Array String -> Set String -> Set String -> Array String -> Array String
    go remaining visited visiting acc =
      case Array.uncons remaining of
        Nothing -> acc
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
      then { visited, visiting, acc }  -- Circular dep
      else
        let visiting' = Set.insert path visiting
            deps = case Map.lookup path graph of
              Just d -> d
              Nothing -> []
            validDeps = Array.filter (\d -> Map.member d graph) deps
            result = foldl (\r d -> visit d r.visited r.visiting r.acc) { visited, visiting: visiting', acc } validDeps
            visited' = Set.insert path result.visited
            visiting'' = Set.delete path result.visiting
        in { visited: visited', visiting: visiting'', acc: Array.snoc result.acc path }

-- ============================================================================
-- Module Compilation
-- ============================================================================

-- | Get module name from file path
getModuleName :: Config -> String -> String
getModuleName config path =
  let baseLen = if String.take (String.length config.libBase) path == config.libBase
                then String.length config.libBase
                else if String.take (String.length config.srcBase) path == config.srcBase
                then String.length config.srcBase
                else 0
      relative = String.drop baseLen path
      withoutExt = String.take (String.length relative - 5) relative
  in String.replaceAll (String.Pattern "/") (String.Replacement ".") withoutExt

-- | Parse and type-check a module
parseAndCheckModule :: Types.ModuleRegistry -> String -> Either String ModuleResult
parseAndCheckModule registry source =
  case CstPipeline.parseModuleCst source of
    Left err -> Left ("Parse error: " <> err)
    Right mod ->
      case TypeChecker.checkModule registry Types.emptyEnv (Array.fromFoldable mod.declarations) of
        Left err -> Left ("Type error: " <> show err)
        Right env ->
          let modDeclsArray = Array.fromFoldable mod.declarations
              exports = TypeChecker.extractExports modDeclsArray
              exportsWithValues = TypeChecker.addValuesToExports exports env modDeclsArray
          in Right { mod, env, exports: exportsWithValues }

-- ============================================================================
-- Main Regeneration Logic
-- ============================================================================

-- | Main entry point: regenerate all compiler modules
regenerate :: FileSystem -> Config -> RegenerateResult
regenerate fs config =
  let libFiles = fs.listFiles config.libBase ".purs"
      srcFiles = fs.listFiles config.srcBase ".purs"
      allFiles = libFiles <> srcFiles
      combinedDeps = buildDependencyGraph fs config allFiles
      sortedModules = topologicalSort combinedDeps
      Tuple count logs = compileAllModules fs config sortedModules
  in { success: true, modulesCompiled: count, logs: logs }

-- | Compile all modules, building up registry incrementally
compileAllModules :: FileSystem -> Config -> Array String -> Tuple Int (Array LogEntry)
compileAllModules fs config sortedPaths =
  let initial = { registry: Types.registerModule Types.emptyRegistry "Prelude" Types.preludeExports, count: 0, logs: [] }
      result = foldl (compileOneModule fs config) initial sortedPaths
  in Tuple result.count result.logs

compileOneModule :: FileSystem -> Config -> { registry :: Types.ModuleRegistry, count :: Int, logs :: Array LogEntry } -> String -> { registry :: Types.ModuleRegistry, count :: Int, logs :: Array LogEntry }
compileOneModule fs config acc path =
  case fs.readFile path of
    Nothing -> acc { logs = Array.snoc acc.logs (LogError ("Cannot read: " <> path)) }
    Just source ->
      let fullModName = getModuleName config path
      in case parseAndCheckModule acc.registry source of
        Left err -> acc { logs = Array.snoc acc.logs (LogError (fullModName <> ": " <> err)) }
        Right result ->
          let code = CodeGen.genModule result.mod
              registry' = Types.registerModule acc.registry fullModName result.exports
              modPath = String.replaceAll (String.Pattern ".") (String.Replacement "/") fullModName
              outputFile = config.outputDir <> modPath <> ".core"
              targetFile = config.targetDir <> modPath <> ".core"
              _written1 = fs.writeFile outputFile code
              _written2 = fs.writeFile targetFile code
              lineCount = Array.length (String.split (String.Pattern "\n") code)
              logMsg = "Compiled " <> fullModName <> " (" <> show lineCount <> " lines)"
          in { registry: registry', count: acc.count + 1, logs: Array.snoc acc.logs (LogInfo logMsg) }

-- ============================================================================
-- Logging Helpers
-- ============================================================================

showLogEntry :: LogEntry -> String
showLogEntry (LogInfo msg) = "[INFO] " <> msg
showLogEntry (LogWarning msg) = "[WARN] " <> msg
showLogEntry (LogError msg) = "[ERROR] " <> msg

showLogs :: Array LogEntry -> String
showLogs = String.joinWith "\n" <<< map showLogEntry
