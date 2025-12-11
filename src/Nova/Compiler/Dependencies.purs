module Nova.Compiler.Dependencies where

import Prelude

import Data.Array as Array
import Data.List (List(..), (:))
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Nova.Compiler.Ast (Declaration(..), Expr(..), Pattern(..), TypeExpr(..), DeclId, ManagedDecl, LetBind, CaseClause, DoStatement(..), GuardedExpr, GuardClause(..), Module, FunctionDeclaration, DataType, TypeAlias, TypeClass, TypeClassInstance, ForeignImport, TypeSignature, TypeDeclaration, NewtypeDecl, ImportDeclaration, InfixDecl)

-- ============================================================================
-- Dependency Extraction
-- ============================================================================

-- | Extract all free names referenced by a declaration
-- | These are names that need to be resolved from the environment
getDependencies :: Declaration -> Set String
getDependencies decl = case decl of
  DeclFunction f ->
    let bodyDeps = getExprDeps f.body
        guardDeps = foldl (\acc g -> Set.union acc (getGuardedExprDeps g)) Set.empty f.guards
        paramNames = foldl (\acc p -> Set.union acc (getBoundNames p)) Set.empty f.parameters
    in Set.difference (Set.union bodyDeps guardDeps) paramNames

  DeclDataType d ->
    foldl (\acc c -> Set.union acc (getConstructorDeps c)) Set.empty d.constructors

  DeclTypeAlias a ->
    getTypeExprDeps a.ty

  DeclTypeClass c ->
    foldl (\acc m -> Set.union acc (getTypeExprDeps m.ty)) Set.empty c.methods

  DeclTypeClassInstance i ->
    let tyDeps = getTypeExprDeps i.ty
        methodDeps = foldl (\acc m -> Set.union acc (getExprDeps m.body)) Set.empty i.methods
    in Set.union tyDeps methodDeps

  DeclForeignImport f ->
    getTypeExprDeps f.typeSignature

  DeclTypeSig s ->
    getTypeExprDeps s.ty

  DeclType t ->
    getTypeExprDeps t.typeSignature

  DeclModule m ->
    getModuleDependencies m.declarations

  DeclImport _ ->
    Set.empty  -- Imports don't have dependencies on local names

  DeclNewtype n ->
    getTypeExprDeps n.wrappedType

  DeclInfix _ ->
    Set.empty  -- Infix declarations don't have dependencies

-- | Get dependencies from module declarations
getModuleDependencies :: List Declaration -> Set String
getModuleDependencies decls = foldl (\acc decl' -> Set.union acc (getDependencies decl')) Set.empty decls

-- | Get dependencies from a data constructor
getConstructorDeps :: { name :: String, fields :: List { label :: String, ty :: TypeExpr }, isRecord :: Boolean } -> Set String
getConstructorDeps c = foldl (\acc f -> Set.union acc (getTypeExprDeps f.ty)) Set.empty c.fields

-- | Get dependencies from a type expression
getTypeExprDeps :: TypeExpr -> Set String
getTypeExprDeps ty = case ty of
  TyExprCon name -> Set.singleton name
  TyExprVar _ -> Set.empty  -- Type variables are not external dependencies
  TyExprApp t1 t2 -> Set.union (getTypeExprDeps t1) (getTypeExprDeps t2)
  TyExprArrow t1 t2 -> Set.union (getTypeExprDeps t1) (getTypeExprDeps t2)
  TyExprRecord fields _ ->
    foldl (\acc (Tuple _ t) -> Set.union acc (getTypeExprDeps t)) Set.empty fields
  TyExprForAll _ t -> getTypeExprDeps t
  TyExprConstrained cs t ->
    let constraintDeps = foldl (\acc c -> Set.union acc (Set.singleton c.className)) Set.empty cs
        tyDeps = foldl (\acc c -> Set.union acc (foldl (\acc2 ct -> Set.union acc2 (getTypeExprDeps ct)) Set.empty c.types)) Set.empty cs
    in Set.union constraintDeps (Set.union tyDeps (getTypeExprDeps t))
  TyExprParens t -> getTypeExprDeps t
  TyExprTuple ts -> foldl (\acc t -> Set.union acc (getTypeExprDeps t)) Set.empty ts

-- | Get free variable dependencies from an expression
getExprDeps :: Expr -> Set String
getExprDeps expr = case expr of
  ExprVar name -> Set.singleton name
  ExprQualified ns name -> Set.singleton (ns <> "." <> name)
  ExprLit _ -> Set.empty
  ExprApp e1 e2 -> Set.union (getExprDeps e1) (getExprDeps e2)
  ExprLambda pats body ->
    let boundNames = foldl (\acc p -> Set.union acc (getBoundNames p)) Set.empty pats
    in Set.difference (getExprDeps body) boundNames
  ExprLet binds body ->
    let bindDeps = getLetBindsDeps binds
        boundNames = foldl (\acc b -> Set.union acc (getBoundNames b.pattern)) Set.empty binds
    in Set.difference (Set.union bindDeps (getExprDeps body)) boundNames
  ExprIf c t e -> Set.union (getExprDeps c) (Set.union (getExprDeps t) (getExprDeps e))
  ExprCase e clauses ->
    Set.union (getExprDeps e) (foldl (\acc c -> Set.union acc (getCaseClauseDeps c)) Set.empty clauses)
  ExprDo stmts -> getDoStatementsDeps stmts
  ExprBinOp op e1 e2 -> Set.union (Set.singleton op) (Set.union (getExprDeps e1) (getExprDeps e2))
  ExprUnaryOp op e -> Set.union (Set.singleton op) (getExprDeps e)
  ExprList es -> foldl (\acc e -> Set.union acc (getExprDeps e)) Set.empty es
  ExprTuple es -> foldl (\acc e -> Set.union acc (getExprDeps e)) Set.empty es
  ExprRecord fields -> foldl (\acc (Tuple _ e) -> Set.union acc (getExprDeps e)) Set.empty fields
  ExprRecordAccess e _ -> getExprDeps e
  ExprRecordUpdate e fields -> Set.union (getExprDeps e) (foldl (\acc (Tuple _ v) -> Set.union acc (getExprDeps v)) Set.empty fields)
  ExprTyped e _ -> getExprDeps e
  ExprParens e -> getExprDeps e
  ExprSection _ -> Set.empty  -- Operator sections are handled specially
  ExprSectionLeft e _ -> getExprDeps e  -- Left section like (1 +)
  ExprSectionRight _ e -> getExprDeps e  -- Right section like (+ 1)

-- | Get dependencies from let bindings
getLetBindsDeps :: List LetBind -> Set String
getLetBindsDeps binds = foldl (\acc b -> Set.union acc (getExprDeps b.value)) Set.empty binds

-- | Get dependencies from a case clause
getCaseClauseDeps :: CaseClause -> Set String
getCaseClauseDeps clause =
  let boundNames = getBoundNames clause.pattern
      bodyDeps = getExprDeps clause.body
      guardDeps = case clause.guard of
        Nothing -> Set.empty
        Just g -> getExprDeps g
  in Set.difference (Set.union bodyDeps guardDeps) boundNames

-- | Get dependencies from do statements
getDoStatementsDeps :: List DoStatement -> Set String
getDoStatementsDeps stmts =
  let go remaining bound = case remaining of
        Nil -> Set.empty
        (stmt : rest) ->
          case stmt of
            DoLet binds ->
              let deps = getLetBindsDeps binds
                  newBound = foldl (\acc b -> Set.union acc (getBoundNames b.pattern)) bound binds
              in Set.union (Set.difference deps bound) (go rest newBound)
            DoBind pat e ->
              let deps = getExprDeps e
                  newBound = Set.union bound (getBoundNames pat)
              in Set.union (Set.difference deps bound) (go rest newBound)
            DoExpr e ->
              Set.union (Set.difference (getExprDeps e) bound) (go rest bound)
  in go stmts Set.empty

-- | Get dependencies from a guarded expression
getGuardedExprDeps :: GuardedExpr -> Set String
getGuardedExprDeps g =
  let guardDeps = foldl (\acc gc -> Set.union acc (getGuardClauseDeps gc)) Set.empty g.guards
      bodyDeps = getExprDeps g.body
  in Set.union guardDeps bodyDeps

-- | Get dependencies from a guard clause
getGuardClauseDeps :: GuardClause -> Set String
getGuardClauseDeps gc = case gc of
  GuardExpr e -> getExprDeps e
  GuardPat pat e ->
    let boundNames = getBoundNames pat
    in Set.difference (getExprDeps e) boundNames

-- | Get names bound by a pattern (to exclude from dependencies)
getBoundNames :: Pattern -> Set String
getBoundNames pat = case pat of
  PatVar name -> Set.singleton name
  PatWildcard -> Set.empty
  PatLit _ -> Set.empty
  PatCon _ pats -> foldl (\acc p -> Set.union acc (getBoundNames p)) Set.empty pats
  PatRecord fields -> foldl (\acc (Tuple _ p) -> Set.union acc (getBoundNames p)) Set.empty fields
  PatList pats -> foldl (\acc p -> Set.union acc (getBoundNames p)) Set.empty pats
  PatCons p1 p2 -> Set.union (getBoundNames p1) (getBoundNames p2)
  PatAs name p -> Set.insert name (getBoundNames p)
  PatParens p -> getBoundNames p

-- ============================================================================
-- Dependency Graph
-- ============================================================================

-- | A dependency graph mapping declaration IDs to their dependencies
type DependencyGraph =
  { forward :: Map DeclId (Set DeclId)   -- declId -> what it depends on
  , reverse :: Map DeclId (Set DeclId)   -- declId -> what depends on it
  }

-- | Empty dependency graph
emptyGraph :: DependencyGraph
emptyGraph = { forward: Map.empty, reverse: Map.empty }

-- | Build a dependency graph from managed declarations
-- | This requires a name resolution function to convert names to DeclIds
buildDependencyGraph :: Map DeclId ManagedDecl -> (String -> String -> Maybe DeclId) -> DependencyGraph
buildDependencyGraph decls resolveName =
  let ids = Map.keys decls
      -- Build forward edges
      forward = foldl (\acc id ->
        case Map.lookup id decls of
          Nothing -> acc
          Just md ->
            let deps = getDependencies md.decl
                resolvedDeps = Set.mapMaybe (\name -> resolveName md.meta.namespace name) deps
            in Map.insert id resolvedDeps acc
      ) Map.empty ids
      -- Build reverse edges from forward edges
      reverse = buildReverseEdges forward
  in { forward, reverse }

-- | Build reverse edges from forward edges
buildReverseEdges :: Map DeclId (Set DeclId) -> Map DeclId (Set DeclId)
buildReverseEdges forward =
  let entries = Map.toUnfoldable forward :: Array (Tuple DeclId (Set DeclId))
  in foldl (\acc (Tuple fromId deps) ->
    let depsArray = Set.toUnfoldable deps :: Array DeclId
    in foldl (\acc2 toId ->
      let existing = case Map.lookup toId acc2 of
            Nothing -> Set.empty
            Just s -> s
      in Map.insert toId (Set.insert fromId existing) acc2
    ) acc depsArray
  ) Map.empty entries

-- | Add a declaration to the graph
addToGraph :: DeclId -> Set DeclId -> DependencyGraph -> DependencyGraph
addToGraph declId deps graph =
  let forward = Map.insert declId deps graph.forward
      depsArray = Set.toUnfoldable deps :: Array DeclId
      reverse = foldl (\acc depId ->
        let existing = case Map.lookup depId acc of
              Nothing -> Set.empty
              Just s -> s
        in Map.insert depId (Set.insert declId existing) acc
      ) graph.reverse depsArray
  in { forward, reverse }

-- | Remove a declaration from the graph
removeFromGraph :: DeclId -> DependencyGraph -> DependencyGraph
removeFromGraph declId graph =
  let -- Get what this declaration depended on
      deps = case Map.lookup declId graph.forward of
        Nothing -> Set.empty
        Just s -> s
      depsArray = Set.toUnfoldable deps :: Array DeclId
      -- Remove from forward edges
      forward = Map.delete declId graph.forward
      -- Remove from reverse edges of its dependencies
      reverse = foldl (\acc depId ->
        case Map.lookup depId acc of
          Nothing -> acc
          Just dependents -> Map.insert depId (Set.delete declId dependents) acc
      ) graph.reverse depsArray
      -- Also remove declId's reverse entry
      reverse' = Map.delete declId reverse
  in { forward, reverse: reverse' }

-- | Get all declarations that depend on a given declaration (direct dependents)
getDependents :: DependencyGraph -> DeclId -> Set DeclId
getDependents graph declId = case Map.lookup declId graph.reverse of
  Nothing -> Set.empty
  Just s -> s

-- | Get all declarations that a given declaration depends on
getDependenciesOf :: DependencyGraph -> DeclId -> Set DeclId
getDependenciesOf graph declId = case Map.lookup declId graph.forward of
  Nothing -> Set.empty
  Just s -> s

-- | Get all transitively affected declarations when a declaration changes
-- | This includes direct dependents and their dependents, recursively
getAffected :: DependencyGraph -> DeclId -> Set DeclId
getAffected graph declId =
  let go toProcess visited =
        case Set.findMin toProcess of
          Nothing -> visited
          Just id ->
            if Set.member id visited
            then go (Set.delete id toProcess) visited
            else
              let dependents = getDependents graph id
                  newToProcess = Set.union (Set.delete id toProcess) (Set.difference dependents visited)
              in go newToProcess (Set.insert id visited)
  in go (Set.singleton declId) Set.empty

-- | Topological sort of declarations (for type checking order)
-- | Returns declarations in dependency order (dependencies before dependents)
topologicalSort :: DependencyGraph -> Array DeclId -> Array DeclId
topologicalSort graph ids =
  let -- Kahn's algorithm
      initialInDegree = foldl (\acc id ->
        let deps = getDependenciesOf graph id
            -- Only count dependencies within our set of ids
            relevantDeps = Set.size (Set.intersection deps (Set.fromFoldable ids))
        in Map.insert id relevantDeps acc
      ) Map.empty ids

      go inDegree result queue =
        case Array.uncons queue of
          Nothing -> result
          Just { head: id, tail: restQueue } ->
            let allDependents = Set.toUnfoldable (getDependents graph id) :: Array DeclId
                dependents = Array.filter (\d -> Array.elem d ids) allDependents
                -- Decrease in-degree for each dependent
                newInDegree = foldl (\acc dep ->
                  case Map.lookup dep acc of
                    Nothing -> acc
                    Just n -> Map.insert dep (n - 1) acc
                ) inDegree dependents
                -- Add dependents with in-degree 0 to queue
                newQueue = restQueue <> Array.filter (\d ->
                  case Map.lookup d newInDegree of
                    Nothing -> false
                    Just n -> n == 0 && not (Array.elem d result) && not (Array.elem d queue)
                ) dependents
            in go newInDegree (Array.snoc result id) newQueue

      -- Find initial nodes with no dependencies (in-degree 0)
      initialQueue = Array.filter (\id ->
        case Map.lookup id initialInDegree of
          Nothing -> false
          Just n -> n == 0
      ) ids

  in go initialInDegree [] initialQueue
