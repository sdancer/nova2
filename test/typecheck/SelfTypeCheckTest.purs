module Test.TypeCheck.SelfTypeCheckTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Foldable (foldM)
import Data.Traversable (traverse)
import Control.Monad (void)
import Data.Maybe (Maybe(..))
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Nova.Compiler.Tokenizer (tokenize)
import Nova.Compiler.Parser as P
import Nova.Compiler.Ast (Declaration(..))
import Nova.Compiler.Types (emptyEnv, Env)
import Nova.Compiler.TypeChecker (checkDecl, checkModule, TCError)

main :: Effect Unit
main = do
  log "=== Self Type-Check Coverage Tests ==="
  log ""

  -- Test modules individually first (for modules with no deps)
  testFile "Types.purs" "src/Nova/Compiler/Types.purs"
  testFile "Ast.purs" "src/Nova/Compiler/Ast.purs"
  testFile "Tokenizer.purs" "src/Nova/Compiler/Tokenizer.purs"
  testFile "TypeChecker.purs" "src/Nova/Compiler/TypeChecker.purs"
  testFile "CodeGen.purs" "src/Nova/Compiler/CodeGen.purs"

  -- For modules with dependencies, combine all declarations
  -- This simulates how a real multi-module type checker would work
  testFileCombined "Unify.purs" "src/Nova/Compiler/Unify.purs"
    ["src/Nova/Compiler/Types.purs"]

  testFileCombined "Parser.purs" "src/Nova/Compiler/Parser.purs"
    ["src/Nova/Compiler/Types.purs", "src/Nova/Compiler/Ast.purs", "src/Nova/Compiler/Tokenizer.purs"]

  log ""
  log "=== Tests Complete ==="

-- | Parse a file and return its declarations
parseFile :: String -> Effect (Array Declaration)
parseFile path = do
  content <- readTextFile UTF8 path
  let tokens = tokenize content
  case P.parseModule tokens of
    Left parseErr -> do
      log $ "Parse error in " <> path <> ": " <> parseErr
      pure []
    Right (Tuple m _) -> pure m.declarations

-- | Load multiple module dependencies into an environment
loadModules :: Array String -> Effect Env
loadModules paths = do
  allDecls <- map Array.concat $ traverse parseFile paths
  case checkModule emptyEnv allDecls of
    Left _ -> pure emptyEnv  -- Return empty env if check fails
    Right env -> pure env

-- | Test a file with module dependencies loaded first
testFileWithDeps :: String -> String -> Array String -> Effect Unit
testFileWithDeps name path deps = do
  -- First load dependency modules
  baseEnv <- loadModules deps

  content <- readTextFile UTF8 path
  let tokens = tokenize content
  case P.parseModule tokens of
    Left parseErr -> log $ "✗ " <> name <> " - Parse error: " <> parseErr
    Right (Tuple m _) -> do
      let decls = m.declarations
      let total = Array.length decls

      -- Type check with the dependency env
      case checkModule baseEnv decls of
        Right _ -> do
          log $ name <> ": " <> show total <> "/" <> show total <> " (100%) - Module OK!"
        Left moduleErr -> do
          log $ name <> ": Module error: " <> show moduleErr

          -- Fall back to checking declarations individually
          let results = typeCheckDecls baseEnv decls
          let passed = countPassed results
          let percentage = if total > 0 then (passed * 100) / total else 0

          log $ "  Individual check: " <> show passed <> "/" <> show total <> " (" <> show percentage <> "%)"

          let failures = getFailures results 5
          showFailures failures

-- | Test a file by combining all declarations from deps + main file
-- | This ensures type aliases from deps are available when processing main file's types
testFileCombined :: String -> String -> Array String -> Effect Unit
testFileCombined name path deps = do
  -- Load all dependency declarations
  depDecls <- map Array.concat $ traverse parseFile deps

  -- Load main file declarations
  mainDecls <- parseFile path
  let total = Array.length mainDecls

  -- Combine all declarations
  let allDecls = depDecls <> mainDecls

  -- Type check all declarations together
  case checkModule emptyEnv allDecls of
    Right _ -> do
      log $ name <> ": " <> show total <> "/" <> show total <> " (100%) - Module OK!"
    Left moduleErr -> do
      log $ name <> ": Module error: " <> show moduleErr

      -- Fall back to checking declarations individually (using combined env)
      let baseEnv = case checkModule emptyEnv depDecls of
            Right e -> e
            Left _ -> emptyEnv
      let results = typeCheckDecls baseEnv mainDecls
      let passed = countPassed results
      let percentage = if total > 0 then (passed * 100) / total else 0

      log $ "  Individual check: " <> show passed <> "/" <> show total <> " (" <> show percentage <> "%)"

      let failures = getFailures results 5
      showFailures failures

testFile :: String -> String -> Effect Unit
testFile name path = do
  content <- readTextFile UTF8 path
  let tokens = tokenize content
  case P.parseModule tokens of
    Left parseErr -> log $ "✗ " <> name <> " - Parse error: " <> parseErr
    Right (Tuple m _) -> do
      -- Count declarations
      let decls = m.declarations
      let total = Array.length decls

      -- Try to type check the whole module (handles forward refs)
      case checkModule emptyEnv decls of
        Right _ -> do
          log $ name <> ": " <> show total <> "/" <> show total <> " (100%) - Module OK!"
        Left moduleErr -> do
          -- Show the module-level error
          log $ name <> ": Module error: " <> show moduleErr

          -- Fall back to checking declarations individually
          let results = typeCheckDecls emptyEnv decls
          let passed = countPassed results
          let percentage = if total > 0 then (passed * 100) / total else 0

          log $ "  Individual check: " <> show passed <> "/" <> show total <> " (" <> show percentage <> "%)"

          -- Show first few failures
          let failures = getFailures results 5
          showFailures failures

typeCheckDecls :: Env -> Array Declaration -> Array (Tuple String (Either TCError Env))
typeCheckDecls env decls = Array.foldl checkOne [] decls
  where
    checkOne acc decl =
      let name = getDeclName decl
          result = checkDecl env decl
      in Array.snoc acc (Tuple name result)

countPassed :: Array (Tuple String (Either TCError Env)) -> Int
countPassed results = Array.length (Array.filter isRight results)
  where
    isRight (Tuple _ (Right _)) = true
    isRight _ = false

getFailures :: Array (Tuple String (Either TCError Env)) -> Int -> Array (Tuple String TCError)
getFailures results n = Array.take n (Array.mapMaybe toFailure results)
  where
    toFailure (Tuple name (Left err)) = Just (Tuple name err)
    toFailure _ = Nothing

showFailures :: Array (Tuple String TCError) -> Effect Unit
showFailures failures =
  void $ foldM (\_ (Tuple name err) -> log $ "  ✗ " <> name <> ": " <> show err) unit failures

getDeclName :: Declaration -> String
getDeclName (DeclFunction f) = f.name
getDeclName (DeclTypeSig sig) = sig.name
getDeclName (DeclDataType dt) = dt.name
getDeclName (DeclTypeAlias ta) = ta.name
getDeclName (DeclTypeClass c) = c.name
getDeclName (DeclTypeClassInstance i) = i.className
getDeclName (DeclImport im) = case im.alias of
  Just a -> a
  Nothing -> im.moduleName
getDeclName (DeclModule m) = m.name
getDeclName (DeclForeignImport f) = f.functionName
getDeclName (DeclType t) = t.name
