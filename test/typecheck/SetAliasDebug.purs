module Test.SetAliasDebug where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.Foldable (foldM)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Nova.Compiler.CstPipeline (parseModuleCst)
import Nova.Compiler.TypeChecker (checkModule, extractExportsWithRegistry, addValuesToExports)
import Nova.Compiler.Types (emptyEnv, ModuleRegistry, defaultRegistry, registerModule, lookupModule)
import Nova.Compiler.Types as Types
import Data.Map as Map

type ModuleInfo = { path :: String, name :: String }
type CompileState = { registry :: ModuleRegistry, ok :: Boolean }

compileModule :: CompileState -> ModuleInfo -> Effect CompileState
compileModule state modInfo = do
  content <- readTextFile UTF8 modInfo.path
  case parseModuleCst content of
    Left err -> do
      log $ "  FAIL parse " <> modInfo.name <> ": " <> err
      pure state { ok = false }
    Right m -> do
      let decls = Array.fromFoldable m.declarations
      let initialExports = extractExportsWithRegistry state.registry decls
      case checkModule state.registry emptyEnv decls of
        Left err -> do
          log $ "  FAIL tc " <> modInfo.name <> ": " <> show err
          let newReg = registerModule state.registry modInfo.name initialExports
          pure state { registry = newReg, ok = false }
        Right env -> do
          log $ "  OK " <> modInfo.name
          let fullExports = addValuesToExports initialExports env decls
          let newReg = registerModule state.registry modInfo.name fullExports
          pure state { registry = newReg }

main :: Effect Unit
main = do
  log "=== Set Type Alias Debug Test ==="
  log ""
  log "1. Building registry with Data.Set only..."

  let baseModules =
        [ { path: "lib/Data/Maybe.purs", name: "Data.Maybe" }
        , { path: "lib/Data/Set.purs", name: "Data.Set" }
        ]

  { registry: reg, ok: _ } <- foldM compileModule { registry: defaultRegistry, ok: true } baseModules

  log ""
  log "2. Testing function with Set.Set return type..."

  -- Minimal test: function returning Set.Set String
  let testSrc1 = """module Test.Mini1 where

import Data.Set as Set

testFn :: Int -> Set.Set String
testFn _ = Set.empty
"""
  case parseModuleCst testSrc1 of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg emptyEnv testDecls of
        Left err -> log $ "  FAIL test1: " <> show err
        Right _ -> log "  OK test1: Set.Set String return type works"

  log ""
  log "3. Testing with Nova.Compiler.Types..."

  -- Build registry with Types module
  let typesModules =
        [ { path: "lib/Data/Tuple.purs", name: "Data.Tuple" }
        , { path: "lib/Data/Either.purs", name: "Data.Either" }
        , { path: "lib/Data/Char.purs", name: "Data.Char" }
        , { path: "lib/Data/Int.purs", name: "Data.Int" }
        , { path: "lib/Data/Number.purs", name: "Data.Number" }
        , { path: "lib/Data/Array.purs", name: "Data.Array" }
        , { path: "lib/Data/List.purs", name: "Data.List" }
        , { path: "lib/Data/String.purs", name: "Data.String" }
        , { path: "lib/Data/String/CodeUnits.purs", name: "Data.String.CodeUnits" }
        , { path: "lib/Data/Foldable.purs", name: "Data.Foldable" }
        , { path: "lib/Data/Map.purs", name: "Data.Map" }
        , { path: "lib/Control/Lazy.purs", name: "Control.Lazy" }
        , { path: "src/Nova/Compiler/Ast.purs", name: "Nova.Compiler.Ast" }
        , { path: "src/Nova/Compiler/Types.purs", name: "Nova.Compiler.Types" }
        ]

  { registry: reg2, ok: _ } <- foldM compileModule { registry: reg, ok: true } typesModules

  log ""
  log "4. Checking what TCon looks like in Types exports..."

  -- Check if TCon is in exports
  case lookupModule reg2 "Nova.Compiler.Types" of
    Nothing -> log "  ERROR: Nova.Compiler.Types not in registry"
    Just typesExports -> do
      log $ "  typeAliases keys: " <> show (Array.fromFoldable (Map.keys typesExports.typeAliases))
      log $ "  expandedTypeAliases keys: " <> show (Array.fromFoldable (Map.keys typesExports.expandedTypeAliases))
      case Map.lookup "TCon" typesExports.expandedTypeAliases of
        Nothing -> log "  WARNING: TCon not in expandedTypeAliases"
        Just _ -> log "  TCon found in expandedTypeAliases"
      case Map.lookup "TCon" typesExports.typeAliases of
        Nothing -> log "  WARNING: TCon not in typeAliases"
        Just _ -> log "  TCon found in typeAliases (unexpanded)"

  log ""
  log "5a. Testing TCon field access directly..."

  -- Test accessing TCon fields directly (not via pattern match)
  let testSrc5a = """module Test.Mini5a where

import Nova.Compiler.Types (TCon)

getName :: TCon -> String
getName tc = tc.name
"""
  case parseModuleCst testSrc5a of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test5a (direct TCon field): " <> show err
        Right _ -> log "  OK test5a: direct TCon.name field access works!"

  log ""
  log "5b. Testing TyCon constructor pattern binding..."

  -- Test just the pattern binding, no field access
  let testSrc5b = """module Test.Mini5b where

import Nova.Compiler.Types (Type(..))

getTyCon :: Type -> Int
getTyCon (TyCon tc) = 42
getTyCon _ = 0
"""
  case parseModuleCst testSrc5b of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test5b (TyCon pattern): " <> show err
        Right _ -> log "  OK test5b: TyCon pattern works!"

  log ""
  log "5c. Testing TyCon pattern with field access..."

  -- Test pattern binding with field access
  let testSrc5c = """module Test.Mini5c where

import Nova.Compiler.Types (Type(..), TCon)

getNameFromType :: Type -> String
getNameFromType (TyCon tc) = tc.name
getNameFromType _ = ""
"""
  case parseModuleCst testSrc5c of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test5c (TyCon pattern + field): " <> show err
        Right _ -> log "  OK test5c: TyCon pattern + field access works!"

  log ""
  log "5d. Testing pattern + Set.empty..."

  -- Test TyCon pattern with just Set.empty (no field access in result)
  let testSrc5d = """module Test.Mini5d where

import Data.Set as Set
import Nova.Compiler.Types (Type(..))

collectNames :: Type -> Set.Set String
collectNames (TyCon tc) = Set.empty
collectNames _ = Set.empty
"""
  case parseModuleCst testSrc5d of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test5d: " <> show err
        Right _ -> log "  OK test5d: pattern + Set.empty works!"

  log ""
  log "5e. Testing pattern + Set.singleton with tc.name..."

  -- This mimics collectTypeNames - uses Type and Set.Set
  let testSrc2 = """module Test.Mini2 where

import Data.Set as Set
import Nova.Compiler.Types (Type(..))

collectNames :: Type -> Set.Set String
collectNames (TyCon tc) = Set.singleton tc.name
collectNames _ = Set.empty
"""
  case parseModuleCst testSrc2 of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test5e (Set.singleton tc.name): " <> show err
        Right _ -> log "  OK test5e: Set.singleton tc.name works!"

  log ""
  log "6a. Testing with just Scheme import (no usage)..."

  -- Import Scheme but don't use it
  let testSrc6a = """module Test.Mini6a where

import Data.Set as Set
import Nova.Compiler.Types (Type(..), Scheme)

collectNames :: Type -> Set.Set String
collectNames (TyCon tc) = Set.singleton tc.name
collectNames _ = Set.empty
"""
  case parseModuleCst testSrc6a of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test6a (import Scheme): " <> show err
        Right _ -> log "  OK test6a: import Scheme works!"

  log ""
  log "6b. Testing with mkScheme import..."

  -- Import mkScheme too
  let testSrc6b = """module Test.Mini6b where

import Data.Set as Set
import Nova.Compiler.Types (Type(..), Scheme, mkScheme)

collectNames :: Type -> Set.Set String
collectNames (TyCon tc) = Set.singleton tc.name
collectNames _ = Set.empty
"""
  case parseModuleCst testSrc6b of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test6b (import mkScheme): " <> show err
        Right _ -> log "  OK test6b: import mkScheme works!"

  log ""
  log "6c. Testing all Type constructors (no Scheme) - multi-clause..."

  -- Test all Type constructors without Scheme - multi-clause (will be merged)
  let testSrc6c = """module Test.Mini6c where

import Data.Set as Set
import Nova.Compiler.Types (Type(..))

collectNames :: Type -> Set.Set String
collectNames (TyVar _) = Set.empty
collectNames (TyCon tc) = Set.singleton tc.name
collectNames (TyRecord _) = Set.empty
collectNames (TyApp _ _) = Set.empty
"""
  case parseModuleCst testSrc6c of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test6c (multi-clause): " <> show err
        Right _ -> log "  OK test6c: multi-clause works!"

  log ""
  log "6c1a. Testing TyVar + wildcard..."

  -- Test TyVar pattern with wildcard
  let testSrc6c1a = """module Test.Mini6c1a where

import Data.Set as Set
import Nova.Compiler.Types (Type(..))

collectNames :: Type -> Set.Set String
collectNames t = case t of
  TyVar _ -> Set.empty
  _ -> Set.empty
"""
  case parseModuleCst testSrc6c1a of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test6c1a (TyVar + wildcard): " <> show err
        Right _ -> log "  OK test6c1a: TyVar + wildcard works!"

  log ""
  log "6c1b. Testing 2 different constructors (TyVar + TyCon only)..."

  -- Test with just 2 different constructors, no wildcard
  let testSrc6c1b = """module Test.Mini6c1b where

import Data.Set as Set
import Nova.Compiler.Types (Type(..))

collectNames :: Type -> Set.Set String
collectNames t = case t of
  TyVar _ -> Set.empty
  TyCon tc -> Set.singleton tc.name
"""
  case parseModuleCst testSrc6c1b of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test6c1b (2 diff ctors): " <> show err
        Right _ -> log "  OK test6c1b: 2 different constructors works!"

  log ""
  log "6c1c. Testing TyVar + TyCon without field access..."

  -- Test without field access - just return Set.empty
  let testSrc6c1c = """module Test.Mini6c1c where

import Data.Set as Set
import Nova.Compiler.Types (Type(..))

collectNames :: Type -> Set.Set String
collectNames t = case t of
  TyVar _ -> Set.empty
  TyCon _ -> Set.empty
"""
  case parseModuleCst testSrc6c1c of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test6c1c (no field access): " <> show err
        Right _ -> log "  OK test6c1c: TyVar + TyCon without field access works!"

  log ""
  log "6c1d. Testing TyVar + TyCon returning String (not Set)..."

  -- Test returning String instead of Set
  let testSrc6c1d = """module Test.Mini6c1d where

import Nova.Compiler.Types (Type(..))

collectNames :: Type -> String
collectNames t = case t of
  TyVar _ -> ""
  TyCon tc -> tc.name
"""
  case parseModuleCst testSrc6c1d of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test6c1d (String result): " <> show err
        Right _ -> log "  OK test6c1d: TyVar + TyCon with String result works!"

  log ""
  log "6c1e. Testing TyVar + TyCon returning Int (field .id on TVar)..."

  -- Test accessing .id field on TyVar bound var
  let testSrc6c1e = """module Test.Mini6c1e where

import Nova.Compiler.Types (Type(..))

collectIds :: Type -> Int
collectIds t = case t of
  TyVar tv -> tv.id
  TyCon _ -> 0
"""
  case parseModuleCst testSrc6c1e of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test6c1e (TyVar field): " <> show err
        Right _ -> log "  OK test6c1e: TyVar field access works!"

  log ""
  log "6c1f. Testing TyVar + TyCon with Set.empty but binding tc..."

  -- Use Set.empty but bind tc (to force field type to be available)
  let testSrc6c1f = """module Test.Mini6c1f where

import Data.Set as Set
import Nova.Compiler.Types (Type(..))

collectNames :: Type -> Set.Set String
collectNames t = case t of
  TyVar _ -> Set.empty
  TyCon tc -> Set.empty
"""
  case parseModuleCst testSrc6c1f of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test6c1f (bind tc, no use): " <> show err
        Right _ -> log "  OK test6c1f: binding tc without using it works!"

  log ""
  log "6c1g. Testing TyVar + TyCon with Set.insert tc.name..."

  -- Use Set.insert instead of Set.singleton
  let testSrc6c1g = """module Test.Mini6c1g where

import Data.Set as Set
import Nova.Compiler.Types (Type(..))

collectNames :: Type -> Set.Set String
collectNames t = case t of
  TyVar _ -> Set.empty
  TyCon tc -> Set.insert tc.name Set.empty
"""
  case parseModuleCst testSrc6c1g of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test6c1g (Set.insert): " <> show err
        Right _ -> log "  OK test6c1g: Set.insert tc.name works!"

  log ""
  log "6c1h. Testing TyVar + TyCon with Array [tc.name]..."

  -- Try using Array literal instead of Set
  let testSrc6c1h = """module Test.Mini6c1h where

import Nova.Compiler.Types (Type(..))

collectNames :: Type -> Array String
collectNames t = case t of
  TyVar _ -> []
  TyCon tc -> [tc.name]
"""
  case parseModuleCst testSrc6c1h of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test6c1h (Array literal): " <> show err
        Right _ -> log "  OK test6c1h: Array literal with tc.name works!"

  log ""
  log "6c1i. Testing reverse order: TyCon first, then TyVar..."

  -- Try TyCon clause first
  let testSrc6c1i = """module Test.Mini6c1i where

import Data.Set as Set
import Nova.Compiler.Types (Type(..))

collectNames :: Type -> Set.Set String
collectNames t = case t of
  TyCon tc -> Set.singleton tc.name
  TyVar _ -> Set.empty
"""
  case parseModuleCst testSrc6c1i of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test6c1i (reverse order): " <> show err
        Right _ -> log "  OK test6c1i: reverse order (TyCon first) works!"

  log ""
  log "6c1j. Testing TyRecord first, then TyCon..."

  -- Try TyRecord clause first instead of TyVar
  let testSrc6c1j = """module Test.Mini6c1j where

import Data.Set as Set
import Nova.Compiler.Types (Type(..))

collectNames :: Type -> Set.Set String
collectNames t = case t of
  TyRecord _ -> Set.empty
  TyCon tc -> Set.singleton tc.name
"""
  case parseModuleCst testSrc6c1j of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test6c1j (TyRecord first): " <> show err
        Right _ -> log "  OK test6c1j: TyRecord first works!"

  log ""
  log "6c1k. Testing TyApp first, then TyCon..."

  -- Try TyApp clause first
  let testSrc6c1k = """module Test.Mini6c1k where

import Data.Set as Set
import Nova.Compiler.Types (Type(..))

collectNames :: Type -> Set.Set String
collectNames t = case t of
  TyApp _ _ -> Set.empty
  TyCon tc -> Set.singleton tc.name
"""
  case parseModuleCst testSrc6c1k of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test6c1k (TyApp first): " <> show err
        Right _ -> log "  OK test6c1k: TyApp first works!"

  log ""
  log "6c1l. Testing with own simple data type..."

  -- Define a simple data type and test
  let testSrc6c1l = """module Test.Mini6c1l where

import Data.Set as Set

data MyType = MyA { id :: Int } | MyB { name :: String }

getName :: MyType -> Set.Set String
getName t = case t of
  MyA _ -> Set.empty
  MyB b -> Set.singleton b.name
"""
  case parseModuleCst testSrc6c1l of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test6c1l (own data type): " <> show err
        Right _ -> log "  OK test6c1l: own data type works!"

  log ""
  log "6c1m. Testing with Set but simple data type..."

  -- Use Maybe as the first constructor instead
  let testSrc6c1m = """module Test.Mini6c1m where

import Data.Set as Set
import Data.Maybe (Maybe(..))

getName :: Maybe { name :: String } -> Set.Set String
getName t = case t of
  Nothing -> Set.empty
  Just r -> Set.singleton r.name
"""
  case parseModuleCst testSrc6c1m of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test6c1m (Maybe): " <> show err
        Right _ -> log "  OK test6c1m: Maybe with Set.singleton works!"

  log ""
  log "6c1n. Testing with Set.singleton and String literal..."

  -- Just use a String literal, not field access
  let testSrc6c1n = """module Test.Mini6c1n where

import Data.Set as Set
import Nova.Compiler.Types (Type(..))

collectNames :: Type -> Set.Set String
collectNames t = case t of
  TyVar _ -> Set.empty
  TyCon _ -> Set.singleton "test"
"""
  case parseModuleCst testSrc6c1n of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test6c1n (literal): " <> show err
        Right _ -> log "  OK test6c1n: Set.singleton with literal works!"

  log ""
  log "6c1o. Testing with let binding before Set.singleton..."

  -- Use let binding to separate field access from Set.singleton
  let testSrc6c1o = """module Test.Mini6c1o where

import Data.Set as Set
import Nova.Compiler.Types (Type(..))

collectNames :: Type -> Set.Set String
collectNames t = case t of
  TyVar _ -> Set.empty
  TyCon tc ->
    let n = tc.name
    in Set.singleton n
"""
  case parseModuleCst testSrc6c1o of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test6c1o (let binding): " <> show err
        Right _ -> log "  OK test6c1o: let binding before Set.singleton works!"

  log ""
  log "6c1p. Testing with local data type and Set..."

  -- Define everything locally
  let testSrc6c1p = """module Test.Mini6c1p where

import Data.Set as Set

data Foo = FooA | FooB

getName :: Foo -> Set.Set Int
getName f = case f of
  FooA -> Set.empty
  FooB -> Set.singleton 42
"""
  case parseModuleCst testSrc6c1p of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test6c1p (local data): " <> show err
        Right _ -> log "  OK test6c1p: local data type + Set works!"

  log ""
  log "6c1q. Testing with Either pattern and Set..."

  -- Use standard Either
  let testSrc6c1q = """module Test.Mini6c1q where

import Data.Set as Set
import Data.Either (Either(..))

getName :: Either Int String -> Set.Set String
getName e = case e of
  Left _ -> Set.empty
  Right s -> Set.singleton s
"""
  case parseModuleCst testSrc6c1q of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test6c1q (Either): " <> show err
        Right _ -> log "  OK test6c1q: Either + Set works!"

  log ""
  log "6c2. Testing 3 Type constructors (explicit case)..."

  -- Test with explicit case expression - only 3 clauses
  let testSrc6c2 = """module Test.Mini6c2 where

import Data.Set as Set
import Nova.Compiler.Types (Type(..))

collectNames :: Type -> Set.Set String
collectNames t = case t of
  TyVar _ -> Set.empty
  TyCon tc -> Set.singleton tc.name
  _ -> Set.empty
"""
  case parseModuleCst testSrc6c2 of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test6c2 (3 clauses): " <> show err
        Right _ -> log "  OK test6c2: 3 clauses works!"

  log ""
  log "6c3. Testing 4 Type constructors (explicit case)..."

  -- Test with explicit case expression - 4 clauses
  let testSrc6c3 = """module Test.Mini6c3 where

import Data.Set as Set
import Nova.Compiler.Types (Type(..))

collectNames :: Type -> Set.Set String
collectNames t = case t of
  TyVar _ -> Set.empty
  TyCon tc -> Set.singleton tc.name
  TyRecord _ -> Set.empty
  TyApp _ _ -> Set.empty
"""
  case parseModuleCst testSrc6c3 of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test6c3 (4 clauses): " <> show err
        Right _ -> log "  OK test6c3: 4 clauses works!"

  log ""
  log "6d. Testing all Type constructors WITH Scheme (multi-clause)..."

  -- Add Scheme to the mix
  let testSrc6d = """module Test.Mini6d where

import Data.Set as Set
import Nova.Compiler.Types (Type(..), Scheme, mkScheme)

collectNames :: Type -> Set.Set String
collectNames (TyVar _) = Set.empty
collectNames (TyCon tc) = Set.singleton tc.name
collectNames (TyRecord _) = Set.empty
collectNames (TyApp _ _) = Set.empty

-- Also use Scheme
testScheme :: Scheme -> Int
testScheme s = 42
"""
  case parseModuleCst testSrc6d of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test6d (multi-clause + Scheme): " <> show err
        Right _ -> log "  OK test6d: multi-clause + Scheme works!"

  log ""
  log "7. Testing Scheme destructuring (was test4)..."

  -- Scheme destructuring like in instantiate
  let testSrc4 = """module Test.Mini4 where

import Data.Set as Set
import Nova.Compiler.Types (Type(..), Scheme, mkScheme, TVar)
import Data.Array as Array

instantiate :: Scheme -> Type
instantiate scheme =
  let { ty: schemeTy, vars: schemeVars } = scheme
  in schemeTy
"""
  case parseModuleCst testSrc4 of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg2 emptyEnv testDecls of
        Left err -> log $ "  FAIL test4 (Scheme destructure): " <> show err
        Right _ -> log "  OK test4: Scheme destructure works!"

  log ""
  log "Done!"
