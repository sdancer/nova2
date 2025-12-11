module Test.TypeCheck.DebugTypesExports where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Nova.Compiler.CstPipeline (parseModuleCst)
import Nova.Compiler.TypeChecker (extractExports, addValuesToExports, checkModule)
import Nova.Compiler.Types (emptyEnv, defaultRegistry)

main :: Effect Unit
main = do
  log "=== Debug Types.purs Exports ==="

  content <- readTextFile UTF8 "src/Nova/Compiler/Types.purs"
  case parseModuleCst content of
    Left err -> log $ "Parse error: " <> err
    Right m -> do
      let decls = Array.fromFoldable m.declarations
      let exports = extractExports decls

      log "\n--- Initial Exports ---"
      log $ "Type aliases: " <> show (Array.fromFoldable (Map.keys exports.typeAliases))
      log $ "Values: " <> show (Array.length (Array.fromFoldable (Map.keys exports.values))) <> " items"
      log $ "Constructors: " <> show (Array.fromFoldable (Map.keys exports.constructors))
      log $ "Types: " <> show (Array.fromFoldable (Map.keys exports.types))

      log "\n--- Checking for Scheme ---"
      log $ "Scheme in typeAliases: " <> show (Map.member "Scheme" exports.typeAliases)
      log $ "Scheme in values: " <> show (Map.member "Scheme" exports.values)
      log $ "Scheme in constructors: " <> show (Map.member "Scheme" exports.constructors)

      -- Now typecheck and add values
      case checkModule defaultRegistry emptyEnv decls of
        Left err -> log $ "Typecheck failed: " <> show err
        Right env -> do
          let exports' = addValuesToExports exports env decls
          log "\n--- After addValuesToExports ---"
          log $ "Type aliases: " <> show (Array.fromFoldable (Map.keys exports'.typeAliases))
          log $ "Expanded aliases: " <> show (Array.length (Array.fromFoldable (Map.keys exports'.expandedTypeAliases))) <> " items"
          log $ "Scheme in expandedTypeAliases: " <> show (Map.member "Scheme" exports'.expandedTypeAliases)
