module Nova.Compiler.TypeChecker where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Data.Array ((:))
import Data.Array as Array
import Data.List (List(..))
import Data.List as List
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Nova.Compiler.Types (Type(..), TVar, Scheme, Env, Subst, emptySubst, composeSubst, applySubst, applySubstToEnv, freeTypeVars, freeTypeVarsEnv, freshVar, extendEnv, lookupEnv, extendTypeAlias, lookupTypeAlias, mkScheme, mkTVar, mkTCon, tInt, tString, tChar, tBool, tArrow, tArray, tTuple, TypeAliasInfo, ModuleExports, ModuleRegistry, emptyExports, lookupModule, mergeExportsToEnv, mergeSelectedExports, mergeTypeExport, TypeInfo, TypedModule, mkTypedModule, getAliasInfoParams, getAliasInfoBody)
import Nova.Compiler.Types as Types
import Nova.Compiler.Ast (Expr(..), Literal(..), Pattern(..), LetBind, CaseClause, Declaration(..), FunctionDeclaration, DoStatement(..), DataType, DataConstructor, DataField, TypeExpr(..), TypeAlias, TypeClass, ImportItem(..), ImportSpec(..), ImportDeclaration, NewtypeDecl, Module, GuardedExpr, GuardClause(..))
import Nova.Compiler.Unify (UnifyError, unify, unifyWithAliases)
import Nova.Compiler.ImportProcessor as IP

-- | FFI helper for converting Int to String
-- This avoids shadowing issues when a local 'show' instance is defined
intToString :: Int -> String
intToString = show

-- | Type checking error
data TCError
  = UnifyErr UnifyError
  | UnifyErrWithContext UnifyError String  -- UnifyError with context (e.g., function name)
  | UnboundVariable String
  | UndefinedQualifiedImport String String  -- module name, function/value name
  | NotImplemented String

instance showTCError :: Show TCError where
  show (UnifyErr e) = "Unification error: " <> show e
  show (UnifyErrWithContext e ctx) = "Unification error in " <> ctx <> ": " <> show e
  show (UnboundVariable v) = "Unbound variable: " <> v
  show (UndefinedQualifiedImport modName valName) =
    "Undefined qualified import: " <> modName <> "." <> valName
  show (NotImplemented s) = "Not implemented: " <> s

-- | Add context to a TCError
addErrorContext :: String -> TCError -> TCError
addErrorContext ctx (UnifyErr e) = UnifyErrWithContext e ctx
addErrorContext ctx (UnifyErrWithContext e oldCtx) = UnifyErrWithContext e (oldCtx <> " -> " <> ctx)
addErrorContext _ err = err

-- | Wrap an Either with error context
withContext :: forall a. String -> Either TCError a -> Either TCError a
withContext ctx (Left err) = Left (addErrorContext ctx err)
withContext _ (Right x) = Right x

-- | Map with index for Lists
listMapWithIndex :: forall a b. (Int -> a -> b) -> List a -> List b
listMapWithIndex f list = go 0 list
  where
  go _ Nil = Nil
  go i (Cons x xs) = Cons (f i x) (go (i + 1) xs)

-- | Result type for instantiation
type InstResult = { ty :: Type, env :: Env }

-- | Helper function to create InstResult
mkInstResult :: Type -> Env -> InstResult
mkInstResult t e = { ty: t, env: e }

-- | Helper for instantiate - recursively substitutes fresh type variables
instantiateGo :: Type -> Env -> List TVar -> Subst -> InstResult
instantiateGo schemeTy e Nil sub = mkInstResult (applySubst sub schemeTy) e
instantiateGo schemeTy e (Cons v rest) sub =
  let { id: vId, name: vName } = v
      Tuple fresh e' = freshVar e vName
      sub' = Map.insert vId (TyVar fresh) sub
  in instantiateGo schemeTy e' rest sub'

-- | Instantiate a type scheme with fresh variables
instantiate :: Env -> Scheme -> InstResult
instantiate env scheme =
  let { ty: schemeTy, vars: schemeVars } = scheme
  in instantiateGo schemeTy env (List.fromFoldable schemeVars) Map.empty

-- | Generalize a type to a scheme
generalize :: Env -> Type -> Scheme
generalize env ty =
  let envFree = freeTypeVarsEnv env
      tyFree = freeTypeVars ty
      freeIds = Set.toUnfoldable (Set.difference tyFree envFree) :: Array Int
      vars = map (\i -> mkTVar i ("t" <> intToString i)) freeIds
  in mkScheme vars ty

-- | Infer type of a literal
inferLit :: Literal -> Type
inferLit (LitInt _) = tInt
inferLit (LitNumber _) = TyCon (mkTCon "Number" [])
inferLit (LitString _) = tString
inferLit (LitChar _) = tChar
inferLit (LitBool _) = tBool

-- | Result record for type inference
type InferResult = { ty :: Type, sub :: Subst, env :: Env }

-- | Unify with type aliases from environment
unifyEnv :: Env -> Type -> Type -> Either UnifyError Subst
unifyEnv env = unifyWithAliases env.typeAliases

-- | Infer type of an expression (Algorithm W)
infer :: Env -> Expr -> Either TCError InferResult

infer env (ExprLit lit) =
  Right { ty: inferLit lit, sub: emptySubst, env }

infer env (ExprVar name) =
  case lookupEnv env name of
    Nothing -> Left (UnboundVariable name)
    Just scheme ->
      let r = instantiate env scheme
      in Right { ty: r.ty, sub: emptySubst, env: r.env }

infer env (ExprQualified m name) =
  let fullName = m <> "." <> name
  in case lookupEnv env fullName of
    Just scheme ->
      let r = instantiate env scheme
      in Right { ty: r.ty, sub: emptySubst, env: r.env }
    Nothing ->
      -- Try looking up just the name without module prefix
      case lookupEnv env name of
        Just scheme ->
          let r = instantiate env scheme
          in Right { ty: r.ty, sub: emptySubst, env: r.env }
        Nothing -> Left (UndefinedQualifiedImport m name)

infer env (ExprApp f arg) =
  -- Special case: (-n) is parsed as ExprApp(ExprVar "-", n), treat as negation
  case f of
    ExprVar "-" -> case arg of
      ExprLit (LitInt _) -> Right { ty: tInt, sub: emptySubst, env }
      ExprLit (LitNumber _) -> Right { ty: TyCon (mkTCon "Number" []), sub: emptySubst, env }
      ExprParens (ExprLit (LitInt _)) -> Right { ty: tInt, sub: emptySubst, env }
      _ -> inferApp env f arg
    _ -> inferApp env f arg
  where
    inferApp e func a =
      case infer e func of
        Left err -> Left err
        Right r1 ->
          case infer r1.env a of
            Left err -> Left err
            Right r2 ->
              let Tuple tv env3 = freshVar r2.env "r"
                  resultTy = TyVar tv
              in case unifyEnv env3 (applySubst r2.sub r1.ty) (tArrow r2.ty resultTy) of
                Left ue -> Left (UnifyErr ue)
                Right s3 ->
                  let sub = composeSubst s3 (composeSubst r2.sub r1.sub)
                  in Right { ty: applySubst s3 resultTy, sub, env: env3 }

infer env (ExprLambda pats body) =
  case List.uncons pats of
    Nothing -> infer env body
    Just { head: pat, tail: restPats } ->
      let Tuple argTv env1 = freshVar env "a"
          argTy = TyVar argTv
      in case inferPat env1 pat argTy of
        Left e -> Left e
        Right patRes ->
          let innerExpr = if List.null restPats then body else ExprLambda restPats body
          in case infer patRes.env innerExpr of
            Left e -> Left e
            Right bodyRes ->
              let sub = composeSubst bodyRes.sub patRes.sub
                  resultTy = tArrow (applySubst sub argTy) bodyRes.ty
              in Right { ty: resultTy, sub, env: bodyRes.env }

infer env (ExprLet binds body) =
  case inferBinds env binds of
    Left e -> Left e
    Right letRes ->
      case infer letRes.env body of
        Left e -> Left e
        Right bodyRes ->
          Right { ty: bodyRes.ty, sub: composeSubst bodyRes.sub letRes.sub, env: bodyRes.env }

infer env (ExprIf cond then_ else_) =
  case infer env cond of
    Left e -> Left e
    Right condRes ->
      case unifyEnv condRes.env condRes.ty tBool of
        Left ue -> Left (UnifyErr ue)
        Right _ ->
          case infer condRes.env then_ of
            Left e -> Left e
            Right thenRes ->
              case infer thenRes.env else_ of
                Left e -> Left e
                Right elseRes ->
                  case unifyEnv elseRes.env (applySubst elseRes.sub thenRes.ty) elseRes.ty of
                    Left ue -> Left (UnifyErr ue)
                    Right s ->
                      Right { ty: applySubst s elseRes.ty, sub: composeSubst s elseRes.sub, env: elseRes.env }

infer env (ExprCase scrutinee clauses) =
  case infer env scrutinee of
    Left e -> Left e
    Right scrutRes ->
      let Tuple resultTv env2 = freshVar scrutRes.env "case"
          resultTy = TyVar resultTv
      in case inferClauses env2 scrutRes.ty resultTy clauses scrutRes.sub of
        Left e -> Left e
        Right clauseRes ->
          Right { ty: applySubst clauseRes.sub resultTy, sub: clauseRes.sub, env: clauseRes.env }

infer env (ExprBinOp op l r) =
  let mkError = case String.lastIndexOf (String.Pattern ".") op of
        Just idx -> UndefinedQualifiedImport (String.take idx op) (String.drop (idx + 1) op)
        Nothing -> UnboundVariable op
  in case lookupEnv env op of
    Nothing -> Left mkError
    Just scheme ->
      let opInst = instantiate env scheme
      in case infer opInst.env l of
        Left e -> Left e
        Right lRes ->
          case infer lRes.env r of
            Left e -> Left e
            Right rRes ->
              let Tuple resTv env4 = freshVar rRes.env "binop"
              in case unifyEnv env4 (applySubst (composeSubst rRes.sub lRes.sub) opInst.ty) (tArrow lRes.ty (tArrow rRes.ty (TyVar resTv))) of
                Left ue -> Left (UnifyErr ue)
                Right s4 ->
                  let sub = composeSubst s4 (composeSubst rRes.sub lRes.sub)
                  in Right { ty: applySubst s4 (TyVar resTv), sub, env: env4 }

infer env (ExprList elems) =
  let Tuple elemTv env1 = freshVar env "elem"
      elemTy = TyVar elemTv
  in case inferElems env1 elemTy elems of
    Left e -> Left e
    Right res ->
      Right { ty: TyCon (mkTCon "Array" [applySubst res.sub elemTy]), sub: res.sub, env: res.env }

infer env (ExprTuple elems) =
  case inferMany env elems of
    Left e -> Left e
    Right res ->
      -- Use tTuple which normalizes Tuple2 -> Tuple for 2-tuples
      Right { ty: tTuple res.tys, sub: res.sub, env: res.env }

infer env (ExprRecord fields) =
  case inferFields env fields of
    Left e -> Left e
    Right res ->
      Right { ty: TyRecord { fields: Map.fromFoldable res.tys, row: Nothing }, sub: res.sub, env: res.env }

infer env (ExprRecordAccess rec field) =
  case infer env rec of
    Left e -> Left e
    Right recRes ->
      let Tuple resultTv env2 = freshVar recRes.env "field"
          Tuple rowTv env3 = freshVar env2 "row"
          expectedRec = TyRecord { fields: Map.singleton field (TyVar resultTv), row: Just rowTv }
      in case unifyEnv env3 recRes.ty expectedRec of
        Left ue -> Left (UnifyErr ue)
        Right s2 ->
          let sub = composeSubst s2 recRes.sub
          in Right { ty: applySubst sub (TyVar resultTv), sub, env: env3 }

infer env (ExprParens e) = infer env e

infer env (ExprTyped e _) = infer env e

infer env (ExprRecordUpdate rec updates) = inferRecordUpdate env rec (Array.fromFoldable updates)

infer env (ExprUnaryOp op e) = inferUnaryOp env op e

infer env (ExprDo stmts) = inferDo env (Array.fromFoldable stmts)

-- ExprSection is a record accessor section like (.field) or operator section like (+)
-- For record accessor: (.field) :: forall r a. { field :: a | r } -> a
-- For operator section: just look up the operator as a variable
infer env (ExprSection name) =
  if String.take 1 name == "."
  then
    -- Record accessor section (.field)
    let field = String.drop 1 name
        Tuple resultTv env1 = freshVar env "field"
        Tuple rowTv env2 = freshVar env1 "row"
        inputType = TyRecord { fields: Map.singleton field (TyVar resultTv), row: Just rowTv }
        resultType = TyVar resultTv
        sectionType = tArrow inputType resultType
    in Right { ty: sectionType, sub: Map.empty, env: env2 }
  else
    -- Operator section - just look it up as a variable
    case Map.lookup name env.bindings of
      Nothing -> Left (UnboundVariable name)
      Just scheme ->
        let { ty, env: env2 } = instantiate env scheme
        in Right { ty, sub: Map.empty, env: env2 }

infer _ _ = Left (NotImplemented "expression form")

-- | Infer record update: rec { field = value, ... }
inferRecordUpdate :: Env -> Expr -> Array (Tuple String Expr) -> Either TCError InferResult
inferRecordUpdate env rec updates = do
  -- Infer the base record type
  recRes <- infer env rec
  -- Infer each update field
  updateRes <- inferFields recRes.env (List.fromFoldable updates)
  -- The result type is the record with updated fields
  -- For now, we just unify to ensure fields exist
  let Tuple rowVar env2 = freshVar updateRes.env "row"
      updateFields = Map.fromFoldable updateRes.tys
      expectedRec = TyRecord { fields: updateFields, row: Just rowVar }
  case unifyEnv env2 (applySubst updateRes.sub recRes.ty) expectedRec of
    Left ue -> Left (UnifyErr ue)
    Right s ->
      -- Result is the original record type with substitutions applied
      let sub = composeSubst s (composeSubst updateRes.sub recRes.sub)
      in Right { ty: applySubst sub recRes.ty, sub, env: env2 }

-- | Infer unary operator
inferUnaryOp :: Env -> String -> Expr -> Either TCError InferResult
inferUnaryOp env op e =
  -- Handle built-in unary operators FIRST (before looking up binary versions)
  case op of
    "-" -> do
      -- Numeric negation: Int -> Int (don't look up binary -)
      res <- infer env e
      case unifyEnv res.env res.ty tInt of
        Left ue -> Left (UnifyErr ue)
        Right s -> Right { ty: tInt, sub: composeSubst s res.sub, env: res.env }
    "!" -> do
      -- Boolean negation: Bool -> Bool
      res <- infer env e
      case unifyEnv res.env res.ty tBool of
        Left ue -> Left (UnifyErr ue)
        Right s -> Right { ty: tBool, sub: composeSubst s res.sub, env: res.env }
    _ ->
      -- For other unary ops, look up in environment
      case lookupEnv env op of
        Nothing -> Left (UnboundVariable op)
        Just scheme -> do
          let opInst = instantiate env scheme
          res <- infer opInst.env e
          let Tuple resTv env2 = freshVar res.env "unary"
          case unifyEnv env2 opInst.ty (tArrow res.ty (TyVar resTv)) of
            Left ue -> Left (UnifyErr ue)
            Right s ->
              let sub = composeSubst s res.sub
              in Right { ty: applySubst s (TyVar resTv), sub, env: env2 }

-- | Infer do-notation by desugaring to binds
-- do { x <- e1; e2 } ==> e1 >>= \x -> e2
-- do { e1; e2 } ==> e1 >>= \_ -> e2 (or e1 >> e2)
-- do { let x = e1; e2 } ==> let x = e1 in do { e2 }
inferDo :: Env -> Array DoStatement -> Either TCError InferResult
inferDo env stmts = case Array.uncons stmts of
  Nothing -> Left (NotImplemented "empty do block")
  Just { head: stmt, tail: rest } ->
    case stmt of
      DoExpr e ->
        if Array.null rest
        then infer env e  -- Last expression, just infer it
        else do
          -- e1 >> rest  (sequence, ignore result)
          res1 <- infer env e
          restRes <- inferDo res1.env rest
          -- For now, just return the type of the rest
          -- A proper implementation would check for Monad constraint
          Right { ty: restRes.ty, sub: composeSubst restRes.sub res1.sub, env: restRes.env }

      DoBind pat e -> do
        -- e >>= \pat -> rest
        res1 <- infer env e
        -- Create fresh type for the "unwrapped" value
        let Tuple innerTv env1 = freshVar res1.env "inner"
            innerTy = TyVar innerTv
        -- Infer the pattern with the inner type
        patRes <- inferPat env1 pat innerTy
        -- Infer the rest of the do block
        restRes <- inferDo patRes.env rest
        -- The result type should be the same monad as e
        -- For now, just return rest's type
        let sub = composeSubst restRes.sub (composeSubst patRes.sub res1.sub)
        Right { ty: restRes.ty, sub, env: restRes.env }

      DoLet binds -> do
        -- let bindings in do block
        letRes <- inferBinds env binds
        inferDo letRes.env rest

-- | Infer types of multiple expressions
type ManyResult = { tys :: Array Type, sub :: Subst, env :: Env }

-- | Helper for inferMany
inferManyGo :: Env -> List Expr -> List Type -> Subst -> Either TCError ManyResult
inferManyGo e Nil acc sub = Right { tys: Array.fromFoldable (List.reverse acc), sub, env: e }
inferManyGo e (Cons expr rest) acc sub =
  case infer e expr of
    Left err -> Left err
    Right res -> inferManyGo res.env rest (Cons res.ty acc) (composeSubst res.sub sub)

inferMany :: Env -> List Expr -> Either TCError ManyResult
inferMany env exprs = inferManyGo env exprs Nil emptySubst

-- | Infer list element types
type ElemsResult = { sub :: Subst, env :: Env }

-- | Helper for inferElems
inferElemsGo :: Env -> Type -> List Expr -> Subst -> Either TCError ElemsResult
inferElemsGo e _ Nil sub = Right { sub, env: e }
inferElemsGo e eTy (Cons expr rest) sub =
  case infer e expr of
    Left err -> Left err
    Right res ->
      case unifyEnv res.env (applySubst res.sub eTy) res.ty of
        Left ue -> Left (UnifyErr ue)
        Right s2 -> inferElemsGo res.env (applySubst s2 eTy) rest (composeSubst s2 (composeSubst res.sub sub))

inferElems :: Env -> Type -> List Expr -> Either TCError ElemsResult
inferElems env elemTy elems = inferElemsGo env elemTy elems emptySubst

-- | Infer record field types
type FieldsResult = { tys :: List (Tuple String Type), sub :: Subst, env :: Env }

-- | Helper for inferFields
inferFieldsGo :: Env -> List (Tuple String Expr) -> List (Tuple String Type) -> Subst -> Either TCError FieldsResult
inferFieldsGo e Nil acc sub = Right { tys: List.reverse acc, sub, env: e }
inferFieldsGo e (Cons (Tuple name expr) rest) acc sub =
  case infer e expr of
    Left err -> Left err
    Right res -> inferFieldsGo res.env rest (Cons (Tuple name res.ty) acc) (composeSubst res.sub sub)

inferFields :: Env -> List (Tuple String Expr) -> Either TCError FieldsResult
inferFields env fields = inferFieldsGo env fields Nil emptySubst

-- | Infer pattern type and extend environment
type PatResult = { env :: Env, sub :: Subst }

inferPat :: Env -> Pattern -> Type -> Either TCError PatResult
inferPat env (PatVar name) ty =
  let scheme = mkScheme [] ty
      env' = extendEnv env name scheme
  in Right { env: env', sub: emptySubst }

inferPat env PatWildcard _ =
  Right { env, sub: emptySubst }

inferPat env (PatLit lit) ty =
  let litTy = inferLit lit
  in case unifyEnv env ty litTy of
    Left ue -> Left (UnifyErr ue)
    Right s -> Right { env, sub: s }

inferPat env (PatCon conName pats) ty =
  -- Try both qualified and unqualified names for constructor lookup
  let unqualifiedName = case String.lastIndexOf (String.Pattern ".") conName of
        Just idx -> String.drop (idx + 1) conName
        Nothing -> conName
      tryLookup name = lookupEnv env name
      -- Build appropriate error based on whether name is qualified
      mkError = case String.lastIndexOf (String.Pattern ".") conName of
        Just idx -> UndefinedQualifiedImport (String.take idx conName) unqualifiedName
        Nothing -> UnboundVariable conName
  in case tryLookup conName of
    Just scheme ->
      let r = instantiate env scheme
      in inferConPats r.env r.ty pats ty
    Nothing -> case tryLookup unqualifiedName of
      Just scheme ->
        let r = instantiate env scheme
        in inferConPats r.env r.ty pats ty
      Nothing -> Left mkError

inferPat env (PatParens p) ty = inferPat env p ty

inferPat env (PatRecord fields) ty = inferRecordPat env fields ty

inferPat env (PatList pats) ty = inferListPat env pats ty

inferPat env (PatCons hd tl) ty = inferConsPat env hd tl ty

inferPat env (PatAs name pat) ty = do
  -- First infer the inner pattern
  patRes <- inferPat env pat ty
  -- Then bind the name to the whole type
  let scheme = mkScheme [] (applySubst patRes.sub ty)
      env' = extendEnv patRes.env name scheme
  Right { env: env', sub: patRes.sub }

inferPat _ _ _ = Left (NotImplemented "pattern form")

-- | Infer record pattern: { x, y } or { x: pat1, y: pat2 }
-- | Helper for inferRecordPat
inferRecordPatGo :: Type -> Env -> List (Tuple String Pattern) -> Map.Map String Type -> Subst -> Either TCError PatResult
inferRecordPatGo ty e Nil fieldTypes sub = do
  -- Build expected record type and unify
  let Tuple rowVar e' = freshVar e "row"
      expectedRec = TyRecord { fields: fieldTypes, row: Just rowVar }
  case unifyEnv e' (applySubst sub ty) expectedRec of
    Left ue -> Left (UnifyErr ue)
    Right s -> Right { env: e', sub: composeSubst s sub }
inferRecordPatGo ty e (Cons (Tuple label pat) rest) fieldTypes sub = do
    -- Create fresh type for this field
    let Tuple fieldVar e1 = freshVar e ("f_" <> label)
        fieldTy = TyVar fieldVar
    -- Infer the pattern with this field type
    patRes <- inferPat e1 pat fieldTy
    -- Continue with remaining fields
    inferRecordPatGo ty patRes.env rest (Map.insert label (applySubst patRes.sub fieldTy) fieldTypes) (composeSubst patRes.sub sub)

inferRecordPat :: Env -> List (Tuple String Pattern) -> Type -> Either TCError PatResult
inferRecordPat env fields ty = inferRecordPatGo ty env fields Map.empty emptySubst

-- | Infer list pattern: [a, b, c]
inferListPat :: Env -> List Pattern -> Type -> Either TCError PatResult
inferListPat env pats ty = do
  -- Create fresh element type
  let Tuple elemVar env1 = freshVar env "elem"
      elemTy = TyVar elemVar
  -- Unify ty with Array elemTy
  case unifyEnv env1 ty (tArray elemTy) of
    Left ue -> Left (UnifyErr ue)
    Right s1 -> do
      -- Infer each element pattern with the element type
      let elemTy' = applySubst s1 elemTy
      goElems env1 pats elemTy' s1
  where
    goElems e Nil _ sub = Right { env: e, sub }
    goElems e (Cons p rest) eTy sub = do
      patRes <- inferPat e p eTy
      goElems patRes.env rest (applySubst patRes.sub eTy) (composeSubst patRes.sub sub)

-- | Infer cons pattern: (h : t)
inferConsPat :: Env -> Pattern -> Pattern -> Type -> Either TCError PatResult
inferConsPat env hdPat tlPat ty = do
  -- Create fresh element type
  let Tuple elemVar env1 = freshVar env "elem"
      elemTy = TyVar elemVar
  -- Unify ty with Array elemTy
  case unifyEnv env1 ty (tArray elemTy) of
    Left ue -> Left (UnifyErr ue)
    Right s1 -> do
      let elemTy' = applySubst s1 elemTy
          listTy = applySubst s1 (tArray elemTy)
      -- Infer head pattern with element type
      hdRes <- inferPat env1 hdPat elemTy'
      -- Infer tail pattern with list type
      tlRes <- inferPat hdRes.env tlPat (applySubst hdRes.sub listTy)
      Right { env: tlRes.env, sub: composeSubst tlRes.sub (composeSubst hdRes.sub s1) }

-- | Infer constructor pattern arguments
-- | Helper for inferConPats
inferConPatsGo :: Type -> Env -> Type -> List Pattern -> Subst -> Either TCError PatResult
inferConPatsGo resultTy e ty Nil sub =
  case unifyEnv e ty resultTy of
    Left ue -> Left (UnifyErr ue)
    Right s -> Right { env: e, sub: composeSubst s sub }
inferConPatsGo resultTy e ty (Cons p rest) sub =
  case ty of
    TyCon c | c.name == "Fun", Array.length c.args == 2 ->
      case { a: Array.head c.args, b: Array.last c.args } of
        { a: Just argTy, b: Just resTy } ->
          case inferPat e p argTy of
            Left err -> Left err
            Right patRes -> inferConPatsGo resultTy patRes.env resTy rest (composeSubst patRes.sub sub)
        _ -> Left (NotImplemented "malformed function type")
    _ -> Left (NotImplemented "expected function type in constructor")

inferConPats :: Env -> Type -> List Pattern -> Type -> Either TCError PatResult
inferConPats env conTy pats resultTy = inferConPatsGo resultTy env conTy pats emptySubst

-- | Infer let bindings
inferBinds :: Env -> List LetBind -> Either TCError PatResult
inferBinds env binds =
  -- First pass: add all bindings with fresh type variables (for recursive refs)
  let envWithPlaceholders = addBindPlaceholders env binds
  -- Second pass: infer actual types
  in inferBindsPass2 envWithPlaceholders binds emptySubst
  where
    addBindPlaceholders :: Env -> List LetBind -> Env
    addBindPlaceholders e bs = foldl addOne e bs

    addOne :: Env -> LetBind -> Env
    addOne e bind = case bind.pattern of
      PatVar name ->
        let Tuple tv e' = freshVar e ("let_" <> name)
        in extendEnv e' name (mkScheme [] (TyVar tv))
      _ -> e

    inferBindsPass2 :: Env -> List LetBind -> Subst -> Either TCError PatResult
    inferBindsPass2 e Nil sub = Right { env: e, sub }
    inferBindsPass2 e (Cons bind rest) sub =
      case infer e bind.value of
        Left err -> Left err
        Right valRes ->
            case inferPat valRes.env bind.pattern valRes.ty of
              Left err -> Left err
              Right patRes ->
                -- IMPORTANT: Use the original env `e` for generalization, not valRes.env or patRes.env
                -- This ensures lambda parameters don't leak into the outer scope and prevent
                -- proper generalization. The lambda's internal bindings (parameters) should
                -- not affect what gets generalized in the let binding.
                let scheme = generalize e (applySubst (composeSubst patRes.sub valRes.sub) valRes.ty)
                    -- For PatVar, we extend with generalized scheme
                    -- For pattern bindings (Tuple, record, etc), use patRes.env which contains the bindings
                    -- but preserve outer bindings and update counter
                    env3 = case bind.pattern of
                      PatVar name -> extendEnv (e { counter = patRes.env.counter }) name scheme
                      _ ->
                        -- Pattern bindings need to be kept from patRes.env
                        -- Apply substitution to resolve type variables in pattern bindings
                        let patEnv = applySubstToEnv (composeSubst patRes.sub valRes.sub) patRes.env
                        in patEnv { bindings = Map.union patEnv.bindings e.bindings }
                in inferBindsPass2 env3 rest (composeSubst patRes.sub (composeSubst valRes.sub sub))

-- | Convert an expression that's being used as a pattern to an actual Pattern
exprToPattern :: Expr -> Pattern
exprToPattern (ExprVar name) = PatVar name
exprToPattern (ExprApp (ExprVar con) arg) = PatCon con (Cons (exprToPattern arg) Nil)
exprToPattern (ExprApp (ExprApp (ExprVar con) arg1) arg2) = PatCon con (Cons (exprToPattern arg1) (Cons (exprToPattern arg2) Nil))
exprToPattern (ExprLit lit) = PatLit lit
exprToPattern (ExprParens e) = exprToPattern e
exprToPattern _ = PatWildcard

-- | Handle pattern guard expressions recursively
inferGuardExpr :: Env -> Expr -> Either TCError { env :: Env, sub :: Subst }
-- Handle && (composition of guards)
inferGuardExpr e (ExprBinOp "&&" left right) =
  case inferGuardExpr e left of
    Left err -> Left err
    Right leftRes ->
      case inferGuardExpr leftRes.env right of
        Left err -> Left err
        Right rightRes ->
          Right { env: rightRes.env, sub: composeSubst rightRes.sub leftRes.sub }
-- Handle pattern guard: Pat <- Expr
inferGuardExpr e (ExprBinOp "<-" patExpr valExpr) =
  case infer e valExpr of
    Left err -> Left err
    Right valRes ->
      -- Convert the pattern expression to a pattern and infer bindings
      let pat = exprToPattern patExpr
      in case inferPat valRes.env pat valRes.ty of
        Left err -> Left err
        Right patRes -> Right { env: patRes.env, sub: composeSubst patRes.sub valRes.sub }
-- Handle comma-separated guards (treated like &&)
inferGuardExpr e (ExprBinOp "," left right) =
  inferGuardExpr e (ExprBinOp "&&" left right)
-- Regular boolean expression
inferGuardExpr e expr =
  case infer e expr of
    Left err -> Left err
    Right res -> Right { env: res.env, sub: res.sub }

-- | Infer a guard expression (if present)
-- Pattern guards like `Pat <- Expr` need special handling to bind variables
inferGuard :: Env -> Maybe Expr -> Either TCError { env :: Env, sub :: Subst }
inferGuard e Nothing = Right { env: e, sub: emptySubst }
inferGuard e (Just guardExpr) = inferGuardExpr e guardExpr

-- | Helper for inferClauses
inferClausesGo :: Type -> Type -> Env -> List CaseClause -> Subst -> Either TCError PatResult
inferClausesGo _ _ e Nil sub = Right { env: e, sub }
inferClausesGo sTy rTy e (Cons clause rest) sub =
  -- Apply accumulated substitution to scrutinee and result types
  let sTy' = applySubst sub sTy
      rTy' = applySubst sub rTy
    in case inferPat e clause.pattern sTy' of
      Left err -> Left err
      Right patRes ->
        -- IMPORTANT: Apply pattern substitution to the environment
        -- This ensures that pattern variables like 'n' in 'Just n' get their
        -- types resolved from fresh type vars (a0) to concrete types (Int)
        let patEnv = applySubstToEnv patRes.sub patRes.env
        -- Check guard if present (guards should be Bool)
        in case inferGuard patEnv clause.guard of
          Left err -> Left err
          Right guardRes ->
            case infer guardRes.env clause.body of
              Left err -> Left err
              Right bodyRes ->
                case unifyEnv bodyRes.env (applySubst bodyRes.sub rTy') bodyRes.ty of
                  Left ue -> Left (UnifyErr ue)
                  Right s ->
                    let newSub = composeSubst s (composeSubst bodyRes.sub (composeSubst guardRes.sub (composeSubst patRes.sub sub)))
                        -- IMPORTANT: Update counter from bodyRes.env to avoid type variable ID collisions
                        -- We keep original bindings but use the new counter
                        e' = e { counter = bodyRes.env.counter }
                    in inferClausesGo sTy rTy e' rest newSub

-- | Infer case clauses
inferClauses :: Env -> Type -> Type -> List CaseClause -> Subst -> Either TCError PatResult
inferClauses env scrutTy resultTy clauses initSub = inferClausesGo scrutTy resultTy env clauses initSub

-- | Convert a list of guarded expressions to a nested if-else expression
-- | For pattern guards (like `| Just x <- foo`), we convert to case expressions
-- | Input: [{ guards: [cond1], body: body1 }, { guards: [cond2], body: body2 }, { guards: [otherwise], body: body3 }]
-- | Output: if cond1 then body1 else if cond2 then body2 else body3
guardedExprsToIf :: List GuardedExpr -> Expr
guardedExprsToIf Nil = ExprVar "undefined" -- Should not happen
guardedExprsToIf (Cons ge Nil) =
  -- Last guard (usually `| otherwise` or final case)
  -- For pattern guards, wrap in case; otherwise just return body
  wrapWithPatternGuards ge.guards ge.body
guardedExprsToIf (Cons ge rest) =
  -- Convert: | cond = body to: if cond then body else (rest...)
  let elseExpr = guardedExprsToIf rest
      thenBody = wrapWithPatternGuards ge.guards ge.body
      cond = guardsToCondition ge.guards
  in ExprIf cond thenBody elseExpr

-- | Convert guard clauses to a condition expression (combining with &&)
-- | For pattern guards, we treat them as "true" since the pattern match is the check
guardsToCondition :: List GuardClause -> Expr
guardsToCondition Nil = ExprVar "true"
guardsToCondition (Cons gc Nil) = guardClauseToCondExpr gc
guardsToCondition (Cons gc rest) =
  ExprBinOp "&&" (guardClauseToCondExpr gc) (guardsToCondition rest)

-- | Convert a single guard clause to a condition expression
guardClauseToCondExpr :: GuardClause -> Expr
guardClauseToCondExpr (GuardExpr e) = e
guardClauseToCondExpr (GuardPat _ _) = ExprVar "true" -- Pattern guards are checked via case, not condition

-- | Wrap body with pattern guard case expressions if any pattern guards exist
wrapWithPatternGuards :: List GuardClause -> Expr -> Expr
wrapWithPatternGuards guards body =
  let patternGuards = List.filter isPatternGuard guards
  in buildPatternGuardCases patternGuards body
  where
    isPatternGuard (GuardPat _ _) = true
    isPatternGuard _ = false

-- | Build nested case expressions for pattern guards
buildPatternGuardCases :: List GuardClause -> Expr -> Expr
buildPatternGuardCases Nil body = body
buildPatternGuardCases (Cons (GuardPat pat scrutinee) rest) body =
  ExprCase scrutinee (Cons { pattern: pat, body: buildPatternGuardCases rest body, guard: Nothing } Nil)
buildPatternGuardCases (Cons _ rest) body = buildPatternGuardCases rest body

-- | Type check a function declaration
-- For recursive functions, we first add a fresh type var for the function name
checkFunction :: Env -> FunctionDeclaration -> Either TCError { scheme :: Scheme, env :: Env }
checkFunction env func =
  -- First, add function name with fresh type variable (for recursion)
  let Tuple funcTv env1 = freshVar env ("fn_" <> func.name)
      funcTy = TyVar funcTv
      tempScheme = mkScheme [] funcTy
      envWithFunc = extendEnv env1 func.name tempScheme
      -- Build body from guards if present, otherwise use func.body
      body = case func.guards of
        Nil -> func.body
        _ -> guardedExprsToIf func.guards
      -- Build expression from parameters and body
      expr = case func.parameters of
        Nil -> body
        _ -> ExprLambda func.parameters body
  in case infer envWithFunc expr of
    Left e -> Left e
    Right res -> do
      -- Unify inferred type with the placeholder
      case unifyEnv res.env (applySubst res.sub funcTy) res.ty of
        Left ue -> Left (UnifyErr ue)
        Right s ->
          let finalSub = composeSubst s res.sub
              finalTy = applySubst finalSub res.ty
              scheme = generalize res.env finalTy
          in Right { scheme, env: extendEnv res.env func.name scheme }

-- | Type check a declaration
checkDecl :: Env -> Declaration -> Either TCError Env
checkDecl env (DeclFunction func) =
  case checkFunction env func of
    Left e -> Left e
    Right r -> Right r.env

checkDecl env (DeclTypeSig _) = Right env

checkDecl env (DeclDataType dt) = Right (checkDataType env dt)

checkDecl env (DeclTypeAlias ta) = Right (checkTypeAlias env ta)

checkDecl env (DeclTypeClass tc) = Right (checkTypeClass env tc)

-- | Handle infix declarations - register operator as alias for function
checkDecl env (DeclInfix inf) =
  -- Look up the type of the function and register the operator with the same type
  case lookupEnv env inf.functionName of
    Just scheme -> Right (extendEnv env inf.operator scheme)
    Nothing -> Right env  -- Function not yet defined, will be checked later

checkDecl env _ = Right env

-- | Process a type class declaration
-- | Adds each method to the environment with its polymorphic type
checkTypeClass :: Env -> TypeClass -> Env
checkTypeClass env tc =
  Array.foldl addMethod env (Array.fromFoldable tc.methods)
  where
    addMethod e sig =
      -- Convert the type expression to a Type, using class type vars
      let varPairs = Array.mapWithIndex (\i v -> Tuple v (mkTVar (e.counter + i) v)) (Array.fromFoldable tc.typeVars)
          varMap = Map.fromFoldable varPairs
          methodType = typeExprToType varMap sig.ty
          -- Create a scheme with the class type variables quantified
          scheme = mkScheme (map snd varPairs) methodType
      in extendEnv e sig.name scheme

-- | Process a data type declaration
-- | Adds the type constructor and all data constructors to the environment
checkDataType :: Env -> DataType -> Env
checkDataType env dt = checkDataTypeWithAllAliases Map.empty Map.empty env dt

-- | Process a data type declaration with access to type aliases
-- | aliasMap is Map String Type containing type alias expansions
checkDataTypeWithAliases :: Map.Map String Type -> Env -> DataType -> Env
checkDataTypeWithAliases aliasMap env dt = checkDataTypeWithAllAliases aliasMap Map.empty env dt

-- | Process a data type declaration with both simple and parameterized type aliases
checkDataTypeWithAllAliases :: Map.Map String Type -> Map.Map String TypeAliasInfo -> Env -> DataType -> Env
checkDataTypeWithAllAliases aliasMap paramAliasMap env dt =
  let -- Create type variables for the type parameters
      typeVarPairs = Array.mapWithIndex (\i v -> Tuple v (mkTVar (env.counter + i) v)) (Array.fromFoldable dt.typeVars)
      typeVarMap = Map.fromFoldable typeVarPairs
      newCounter = env.counter + List.length dt.typeVars
      env1 = env { counter = newCounter }

      -- The result type is the data type applied to its type variables
      typeArgs = map (\(Tuple _ tv) -> TyVar tv) typeVarPairs
      resultType = TyCon (mkTCon dt.name typeArgs)

      -- Add each constructor to the environment
      addConstructor e con =
        let conType = buildConstructorTypeWithAllAliases aliasMap paramAliasMap typeVarMap con.fields resultType
            conScheme = mkScheme (map snd typeVarPairs) conType
        in extendEnv e con.name conScheme
  in Array.foldl addConstructor env1 (Array.fromFoldable dt.constructors)

-- | Process a newtype declaration with access to type aliases
-- | Newtypes are like data types but with exactly one constructor and one wrapped field
checkNewtypeWithAliases :: Map.Map String Type -> Env -> NewtypeDecl -> Env
checkNewtypeWithAliases aliasMap env nt = checkNewtypeWithAllAliases aliasMap Map.empty env nt

-- | Process a newtype declaration with both simple and parameterized type aliases
checkNewtypeWithAllAliases :: Map.Map String Type -> Map.Map String TypeAliasInfo -> Env -> NewtypeDecl -> Env
checkNewtypeWithAllAliases aliasMap paramAliasMap env nt =
  let -- Create type variables for the type parameters
      typeVarPairs = Array.mapWithIndex (\i v -> Tuple v (mkTVar (env.counter + i) v)) (Array.fromFoldable nt.typeVars)
      typeVarMap = Map.fromFoldable typeVarPairs
      newCounter = env.counter + List.length nt.typeVars
      env1 = env { counter = newCounter }

      -- The result type is the newtype applied to its type variables
      typeArgs = map (\(Tuple _ tv) -> TyVar tv) typeVarPairs
      resultType = TyCon (mkTCon nt.name typeArgs)

      -- The constructor takes the wrapped type and returns the newtype
      wrappedTy = typeExprToTypeWithAllAliases aliasMap paramAliasMap typeVarMap nt.wrappedType
      conType = tArrow wrappedTy resultType
      conScheme = mkScheme (map snd typeVarPairs) conType
  in extendEnv env1 nt.constructor conScheme

-- | Build the type for a data constructor
-- | e.g., Just :: forall a. a -> Maybe a
-- | e.g., Cons :: forall a. a -> List a -> List a
-- | Helper for buildConstructorType
buildConstructorTypeGo :: Map.Map String TVar -> Type -> List DataField -> Type
buildConstructorTypeGo _ resultType Nil = resultType
buildConstructorTypeGo varMap resultType (Cons field rest) =
  let fieldTy = typeExprToType varMap field.ty
  in tArrow fieldTy (buildConstructorTypeGo varMap resultType rest)

buildConstructorType :: Map.Map String TVar -> List DataField -> Type -> Type
buildConstructorType varMap fields resultType = buildConstructorTypeGo varMap resultType fields

-- | Build constructor type with type alias lookup
-- | Helper for buildConstructorTypeWithAliases
buildConstructorTypeWithAliasesGo :: Map.Map String Type -> Map.Map String TVar -> Type -> List DataField -> Type
buildConstructorTypeWithAliasesGo _ _ resultType Nil = resultType
buildConstructorTypeWithAliasesGo aliasMap varMap resultType (Cons field rest) =
  let fieldTy = typeExprToTypeWithAliases aliasMap varMap field.ty
  in tArrow fieldTy (buildConstructorTypeWithAliasesGo aliasMap varMap resultType rest)

buildConstructorTypeWithAliases :: Map.Map String Type -> Map.Map String TVar -> List DataField -> Type -> Type
buildConstructorTypeWithAliases aliasMap varMap fields resultType = buildConstructorTypeWithAliasesGo aliasMap varMap resultType fields

-- | Build constructor type with both simple and parameterized type alias lookup
-- | Helper for buildConstructorTypeWithAllAliases
buildConstructorTypeWithAllAliasesGo :: Map.Map String Type -> Map.Map String TypeAliasInfo -> Map.Map String TVar -> Type -> List DataField -> Type
buildConstructorTypeWithAllAliasesGo _ _ _ resultType Nil = resultType
buildConstructorTypeWithAllAliasesGo aliasMap paramAliasMap varMap resultType (Cons field rest) =
  let fieldTy = typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap field.ty
  in tArrow fieldTy (buildConstructorTypeWithAllAliasesGo aliasMap paramAliasMap varMap resultType rest)

buildConstructorTypeWithAllAliases :: Map.Map String Type -> Map.Map String TypeAliasInfo -> Map.Map String TVar -> List DataField -> Type -> Type
buildConstructorTypeWithAllAliases aliasMap paramAliasMap varMap fields resultType = buildConstructorTypeWithAllAliasesGo aliasMap paramAliasMap varMap resultType fields

-- | Convert a TypeExpr to a Type, looking up type aliases from maps
-- aliasMap: simple (non-parameterized) aliases like TokState = { ... }
-- paramAliasMap: parameterized aliases like ParseResult a = Either String (Tuple a ...)
typeExprToTypeWithAliases :: Map.Map String Type -> Map.Map String TVar -> TypeExpr -> Type
typeExprToTypeWithAliases aliasMap varMap expr =
  typeExprToTypeWithAllAliases aliasMap Map.empty varMap expr

-- | Full type conversion with both simple and parameterized aliases
typeExprToTypeWithAllAliases :: Map.Map String Type -> Map.Map String TypeAliasInfo -> Map.Map String TVar -> TypeExpr -> Type
typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap (TyExprVar name) =
  case Map.lookup name varMap of
    Just tv -> TyVar tv
    Nothing -> TyCon (mkTCon name [])
typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap (TyExprCon name) =
  -- Try both qualified name and unqualified name (strip module prefix)
  let unqualifiedName = case String.lastIndexOf (String.Pattern ".") name of
        Just idx -> String.drop (idx + 1) name
        Nothing -> name
      -- Check if it's a simple type alias we should expand
      tryLookup :: String -> Maybe Type
      tryLookup nm = case Map.lookup nm aliasMap of
        Just ty -> Just ty
        Nothing -> case Map.lookup nm paramAliasMap of
          Just info ->
            let ps = getAliasInfoParams info
                bodyExpr = getAliasInfoBody info
            in if Array.null ps
               then Just (typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap bodyExpr)
               else Nothing
          _ -> Nothing
  in case tryLookup name of
    Just ty -> ty
    Nothing -> case tryLookup unqualifiedName of
      Just ty -> ty
      Nothing ->
        -- Not an alias or needs params, treat as type constructor
        typeExprToType varMap (TyExprCon name)
typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap (TyExprApp f arg) =
  -- Check if this is a parameterized alias application
  case collectTypeApp (TyExprApp f arg) of
    Tuple conName args ->
      case Map.lookup conName paramAliasMap of
        Just info ->
          let ps = getAliasInfoParams info
              bodyExpr = getAliasInfoBody info
          in if Array.length ps == Array.length args
             then -- Found matching parameterized alias, substitute params
               let argTypes = map (typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap) args
                   paramSubst = Map.fromFoldable (Array.zip ps argTypes)
               in substituteTypeExpr aliasMap paramAliasMap paramSubst bodyExpr
             else -- Wrong arity, normal type application
               case typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap f of
                 TyCon tc -> TyCon { name: tc.name, args: Array.snoc tc.args (typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap arg) }
                 other -> other
        _ ->
          -- Not a parameterized alias, normal type application
          case typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap f of
            TyCon tc -> TyCon { name: tc.name, args: Array.snoc tc.args (typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap arg) }
            other -> other
typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap (TyExprArrow a b) =
  tArrow (typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap a) (typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap b)
typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap (TyExprRecord fields maybeRow) =
  let fieldMap = Map.fromFoldable (map (\(Tuple l t) -> Tuple l (typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap t)) fields)
      row = case maybeRow of
        Just r -> case Map.lookup r varMap of
          Just tv -> Just tv
          Nothing -> Nothing
        Nothing -> Nothing
  in TyRecord { fields: fieldMap, row }
typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap (TyExprForAll vars t) =
  -- Add forall-bound type variables to varMap
  let varList = Array.mapWithIndex (\i name -> Tuple name (mkTVar (-(i + 1)) name)) (Array.fromFoldable vars)
      newVarMap = Map.union (Map.fromFoldable varList) varMap
  in typeExprToTypeWithAllAliases aliasMap paramAliasMap newVarMap t
typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap (TyExprConstrained _ t) = typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap t
typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap (TyExprParens t) = typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap t
typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap (TyExprTuple ts) =
  tTuple (Array.fromFoldable (map (typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap) ts))

-- | Convert type expression to type, with access to env for resolving type aliases from imports
typeExprToTypeWithEnv :: Env -> Map.Map String Type -> Map.Map String TypeAliasInfo -> Map.Map String TVar -> TypeExpr -> Type
typeExprToTypeWithEnv env aliasMap paramAliasMap varMap (TyExprVar name) =
  case Map.lookup name varMap of
    Just tv -> TyVar tv
    Nothing -> TyCon (mkTCon name [])
typeExprToTypeWithEnv env aliasMap paramAliasMap varMap (TyExprCon name) =
  -- Try both qualified name and unqualified name (strip module prefix)
  let unqualifiedName = case String.lastIndexOf (String.Pattern ".") name of
        Just idx -> String.drop (idx + 1) name
        Nothing -> name
      -- Check if it's a simple type alias in local maps
      tryLookup :: String -> Maybe Type
      tryLookup nm = case Map.lookup nm aliasMap of
        Just ty -> Just ty
        Nothing -> case Map.lookup nm paramAliasMap of
          Just info ->
            let ps = getAliasInfoParams info
                bodyExpr = getAliasInfoBody info
            in if Array.null ps
               then Just (typeExprToTypeWithEnv env aliasMap paramAliasMap varMap bodyExpr)
               else Nothing
          _ -> Nothing
      -- Check if it's a type alias in the environment (from imported modules)
      -- First check env.typeAliases, then env.bindings
      tryEnvLookup :: String -> Maybe Type
      tryEnvLookup nm = case lookupTypeAlias env nm of
        Just ty -> Just ty
        Nothing -> case lookupEnv env nm of
          Just scheme -> Just scheme.ty
          Nothing -> Nothing
  in case tryLookup name of
    Just ty -> ty
    Nothing -> case tryLookup unqualifiedName of
      Just ty -> ty
      Nothing -> case tryEnvLookup name of
        Just ty -> ty
        Nothing -> case tryEnvLookup unqualifiedName of
          Just ty -> ty
          Nothing ->
            -- Not an alias or needs params, treat as type constructor
            typeExprToType varMap (TyExprCon name)
typeExprToTypeWithEnv env aliasMap paramAliasMap varMap (TyExprApp f arg) =
  -- Check if this is a parameterized alias application
  case collectTypeApp (TyExprApp f arg) of
    Tuple conName args ->
      case Map.lookup conName paramAliasMap of
        Just info | Array.length (getAliasInfoParams info) == Array.length args ->
          -- Found matching parameterized alias, substitute params
          let argTypes = map (typeExprToTypeWithEnv env aliasMap paramAliasMap varMap) args
              paramSubst = Map.fromFoldable (Array.zip (getAliasInfoParams info) argTypes)
          in substituteTypeExpr aliasMap paramAliasMap paramSubst (getAliasInfoBody info)
        _ ->
          -- Not a parameterized alias or wrong arity, normal type application
          case typeExprToTypeWithEnv env aliasMap paramAliasMap varMap f of
            TyCon tc -> TyCon { name: tc.name, args: Array.snoc tc.args (typeExprToTypeWithEnv env aliasMap paramAliasMap varMap arg) }
            other -> other
typeExprToTypeWithEnv env aliasMap paramAliasMap varMap (TyExprArrow a b) =
  tArrow (typeExprToTypeWithEnv env aliasMap paramAliasMap varMap a) (typeExprToTypeWithEnv env aliasMap paramAliasMap varMap b)
typeExprToTypeWithEnv env aliasMap paramAliasMap varMap (TyExprRecord fields maybeRow) =
  let fieldMap = Map.fromFoldable (map (\(Tuple l t) -> Tuple l (typeExprToTypeWithEnv env aliasMap paramAliasMap varMap t)) fields)
      row = case maybeRow of
        Just r -> case Map.lookup r varMap of
          Just tv -> Just tv
          Nothing -> Nothing
        Nothing -> Nothing
  in TyRecord { fields: fieldMap, row }
typeExprToTypeWithEnv env aliasMap paramAliasMap varMap (TyExprForAll vars t) =
  let varList = Array.mapWithIndex (\i name -> Tuple name (mkTVar (-(i + 1)) name)) (Array.fromFoldable vars)
      newVarMap = Map.union (Map.fromFoldable varList) varMap
  in typeExprToTypeWithEnv env aliasMap paramAliasMap newVarMap t
typeExprToTypeWithEnv env aliasMap paramAliasMap varMap (TyExprConstrained _ t) =
  typeExprToTypeWithEnv env aliasMap paramAliasMap varMap t
typeExprToTypeWithEnv env aliasMap paramAliasMap varMap (TyExprParens t) =
  typeExprToTypeWithEnv env aliasMap paramAliasMap varMap t
typeExprToTypeWithEnv env aliasMap paramAliasMap varMap (TyExprTuple ts) =
  tTuple (Array.fromFoldable (map (typeExprToTypeWithEnv env aliasMap paramAliasMap varMap) ts))

-- | Collect type application into constructor name and arguments
-- | e.g., TyExprApp (TyExprApp (TyExprCon "Either") a) b -> ("Either", [a, b])
collectTypeApp :: TypeExpr -> Tuple String (Array TypeExpr)
collectTypeApp (TyExprCon name) = Tuple name []
collectTypeApp (TyExprApp f arg) =
  let Tuple name args = collectTypeApp f
  in Tuple name (Array.snoc args arg)
collectTypeApp _ = Tuple "" []

-- | Substitute type variables in a TypeExpr and convert to Type
-- | paramSubst maps type variable names to their substituted Types
substituteTypeExpr :: Map.Map String Type -> Map.Map String TypeAliasInfo -> Map.Map String Type -> TypeExpr -> Type
substituteTypeExpr aliasMap paramAliasMap paramSubst (TyExprVar name) =
  case Map.lookup name paramSubst of
    Just ty -> ty
    Nothing -> TyCon (mkTCon name [])
substituteTypeExpr aliasMap paramAliasMap paramSubst (TyExprCon name) =
  case Map.lookup name aliasMap of
    Just ty -> ty
    Nothing ->
      -- Fall back to typeExprToType which has hardcoded aliases like Token
      typeExprToType Map.empty (TyExprCon name)
substituteTypeExpr aliasMap paramAliasMap paramSubst (TyExprApp f arg) =
  -- Check if this is a nested parameterized alias
  case collectTypeApp (TyExprApp f arg) of
    Tuple conName args ->
      case Map.lookup conName paramAliasMap of
        Just info | Array.length (getAliasInfoParams info) == Array.length args ->
          let argTypes = map (substituteTypeExpr aliasMap paramAliasMap paramSubst) args
              nestedSubst = Map.fromFoldable (Array.zip (getAliasInfoParams info) argTypes)
          in substituteTypeExpr aliasMap paramAliasMap nestedSubst (getAliasInfoBody info)
        _ ->
          case substituteTypeExpr aliasMap paramAliasMap paramSubst f of
            TyCon tc -> TyCon { name: tc.name, args: Array.snoc tc.args (substituteTypeExpr aliasMap paramAliasMap paramSubst arg) }
            other -> other
substituteTypeExpr aliasMap paramAliasMap paramSubst (TyExprArrow a b) =
  tArrow (substituteTypeExpr aliasMap paramAliasMap paramSubst a) (substituteTypeExpr aliasMap paramAliasMap paramSubst b)
substituteTypeExpr aliasMap paramAliasMap paramSubst (TyExprRecord fields maybeRow) =
  let fieldMap = Map.fromFoldable (map (\(Tuple l t) -> Tuple l (substituteTypeExpr aliasMap paramAliasMap paramSubst t)) fields)
  in TyRecord { fields: fieldMap, row: Nothing }
substituteTypeExpr aliasMap paramAliasMap paramSubst (TyExprTuple ts) =
  tTuple (Array.fromFoldable (map (substituteTypeExpr aliasMap paramAliasMap paramSubst) ts))
substituteTypeExpr aliasMap paramAliasMap paramSubst (TyExprParens t) = substituteTypeExpr aliasMap paramAliasMap paramSubst t
substituteTypeExpr aliasMap paramAliasMap paramSubst (TyExprForAll _ t) = substituteTypeExpr aliasMap paramAliasMap paramSubst t
substituteTypeExpr aliasMap paramAliasMap paramSubst (TyExprConstrained _ t) = substituteTypeExpr aliasMap paramAliasMap paramSubst t

-- | Convert a TypeExpr to a Type using the variable mapping
-- | NOTE: This function only handles primitive type conversions.
-- | For type alias resolution, use typeExprToTypeWithAllAliases with proper alias maps.
typeExprToType :: Map.Map String TVar -> TypeExpr -> Type
typeExprToType varMap (TyExprVar name) =
  case Map.lookup name varMap of
    Just tv -> TyVar tv
    Nothing -> TyCon (mkTCon name [])  -- Assume it's a type constructor
typeExprToType varMap (TyExprCon name) =
  -- Only handle truly primitive cases - type alias resolution happens elsewhere
  case name of
    -- Type wildcard/hole - use a special placeholder type var
    "_" -> TyVar (mkTVar (-999) "_")
    -- PureScript uses Boolean, we use Bool internally
    "Boolean" -> tBool
    -- All other types are treated as type constructors
    -- Type alias expansion should be done via typeExprToTypeWithAllAliases
    _ -> TyCon (mkTCon name [])
typeExprToType varMap (TyExprApp f arg) =
  case typeExprToType varMap f of
    TyCon tc -> TyCon { name: tc.name, args: Array.snoc tc.args (typeExprToType varMap arg) }
    other -> other  -- Shouldn't happen for well-formed types
typeExprToType varMap (TyExprArrow a b) =
  tArrow (typeExprToType varMap a) (typeExprToType varMap b)
typeExprToType varMap (TyExprRecord fields maybeRow) =
  let fieldMap = Map.fromFoldable (map (\(Tuple l t) -> Tuple l (typeExprToType varMap t)) fields)
      row = case maybeRow of
        Just r -> case Map.lookup r varMap of
          Just tv -> Just tv
          Nothing -> Nothing
        Nothing -> Nothing
  in TyRecord { fields: fieldMap, row }
typeExprToType varMap (TyExprForAll vars t) =
  -- Add forall-bound type variables to varMap
  -- Use negative IDs since these are quantified variables that will be instantiated later
  let varList = listMapWithIndex (\i name -> Tuple name (mkTVar (-(i + 1)) name)) vars
      newVarMap = Map.union (Map.fromFoldable varList) varMap  -- New vars shadow old ones
  in typeExprToType newVarMap t
typeExprToType varMap (TyExprConstrained _ t) = typeExprToType varMap t  -- Ignore constraints
typeExprToType varMap (TyExprParens t) = typeExprToType varMap t
typeExprToType varMap (TyExprTuple ts) =
  -- Use tTuple to normalize tuple names (Tuple for 2-tuples)
  tTuple (Array.fromFoldable (map (typeExprToType varMap) ts))

-- | Process a type alias declaration
-- Uses env to resolve imported type aliases in the body
checkTypeAlias :: Env -> TypeAlias -> Env
checkTypeAlias env ta =
  -- Convert the type expression to an actual Type, using env to resolve imports
  let ty = typeExprToTypeWithEnv env Map.empty Map.empty Map.empty ta.ty
      scheme = mkScheme [] ty
      -- Also add to typeAliases map for unification of record types
      env' = extendEnv env ta.name scheme
  in Types.extendTypeAlias env' ta.name ty

-- | Check if a type is a record type
isRecordType :: Type -> Boolean
isRecordType (TyRecord _) = true
isRecordType _ = false

-- | Collect all type constructor names referenced in a type
collectTypeNames :: Type -> Set.Set String
collectTypeNames (TyVar _) = Set.empty
collectTypeNames (TyCon tc) =
  let argsNames = Array.foldl (\s t -> Set.union s (collectTypeNames t)) Set.empty tc.args
  in Set.insert tc.name argsNames
collectTypeNames (TyRecord r) =
  let fieldNames = Map.values r.fields
      fieldsSet = Array.foldl (\s t -> Set.union s (collectTypeNames t)) Set.empty (Array.fromFoldable fieldNames)
  in fieldsSet

-- | Collect all type constructor names referenced in a TypeExpr (before expansion)
collectTypeExprNames :: TypeExpr -> Set.Set String
collectTypeExprNames (TyExprVar _) = Set.empty
collectTypeExprNames (TyExprCon name) = Set.singleton name
collectTypeExprNames (TyExprApp f arg) = Set.union (collectTypeExprNames f) (collectTypeExprNames arg)
collectTypeExprNames (TyExprArrow a b) = Set.union (collectTypeExprNames a) (collectTypeExprNames b)
collectTypeExprNames (TyExprRecord fields _) =
  Array.foldl (\s (Tuple _ t) -> Set.union s (collectTypeExprNames t)) Set.empty (Array.fromFoldable fields)
collectTypeExprNames (TyExprForAll _ t) = collectTypeExprNames t
collectTypeExprNames (TyExprConstrained _ t) = collectTypeExprNames t
collectTypeExprNames (TyExprParens t) = collectTypeExprNames t
collectTypeExprNames (TyExprTuple ts) =
  List.foldl (\s t -> Set.union s (collectTypeExprNames t)) Set.empty ts

-- | Type check a module with imports resolved from a module registry
-- | Pass 1: Process imports and collect type aliases
-- | Pass 2: Process data types, type aliases, and collect function signatures
-- | Pass 3: Type check function bodies
checkModule :: ModuleRegistry -> Env -> Array Declaration -> Either TCError Env
checkModule registry env decls =
  -- First collect imported type aliases
  let importedAliases = collectImportedAliases registry decls
      -- Process imports to add imported types/values to environment (using ImportProcessor)
      env1 = IP.processImports registry env decls
      -- Then proceed with normal type checking, passing imported aliases
      env2 = processNonFunctionsWithAliases importedAliases env1 decls
      env3 = addFunctionPlaceholdersWithAliases importedAliases env2 decls
      -- Process infix declarations after function placeholders are added
      env4 = processInfixDeclarations env3 decls
  in withContext "checkFunctionBodies" (checkFunctionBodies env4 decls)

-- | Type check a full module and return a TypedModule
-- | This is the preferred entry point as it guarantees the module has been validated
typeCheckModule :: ModuleRegistry -> Env -> Module -> Either TCError TypedModule
typeCheckModule registry env mod =
  let decls = Array.fromFoldable mod.declarations
  in case checkModule registry env decls of
    Left err -> Left err
    Right env' -> Right (mkTypedModule mod env')

-- | Process import declarations and add imported types/values to environment
processImports :: ModuleRegistry -> Env -> Array Declaration -> Env
processImports registry env decls =
  Array.foldl processImport env decls
  where
    processImport :: Env -> Declaration -> Env
    processImport e (DeclImport imp) = processImportDecl registry e imp
    processImport e _ = e

-- | Merge exports including type aliases into environment
-- | This expands non-parameterized type aliases and adds them to env.typeAliases
mergeExportsWithTypeAliases :: Env -> ModuleExports -> Env
mergeExportsWithTypeAliases env exports =
  let -- First merge values and constructors using the standard function
      env1 = Types.mergeExportsToEnv env exports
      -- Add RECORD type aliases to env.typeAliases for proper unification
      -- We only add record aliases (not all aliases) to avoid conflicts with data types
      -- Use pre-expanded type aliases from exports if available, otherwise expand ourselves
      moduleAliases = if Map.isEmpty exports.expandedTypeAliases
                      then expandModuleAliases exports.typeAliases
                      else exports.expandedTypeAliases
      addIfRecordAlias e (Tuple name ty) =
        if isRecordType ty then extendTypeAlias e name ty else e
      env2 = Array.foldl addIfRecordAlias env1 (Map.toUnfoldable moduleAliases)
  in env2

-- | Process a single import declaration
processImportDecl :: ModuleRegistry -> Env -> ImportDeclaration -> Env
processImportDecl registry env imp =
  case lookupModule registry imp.moduleName of
    Nothing -> env  -- Module not found, skip (could add warning)
    Just exports ->
      let -- Apply alias prefix if present, or use module name as prefix
          envWithQualified = case imp.alias of
            Just alias -> Types.mergeExportsToEnvWithPrefix env exports alias
            Nothing ->
              -- Extract last component of module name for default qualified access
              -- e.g., Data.List -> List
              let lastPart = case String.lastIndexOf (String.Pattern ".") imp.moduleName of
                    Nothing -> imp.moduleName
                    Just idx -> String.drop (idx + 1) imp.moduleName
              in Types.mergeExportsToEnvWithPrefix env exports lastPart
      in
        if imp.hiding then
          -- Import everything EXCEPT the listed items (qualified access already added)
          mergeExportsWithTypeAliases envWithQualified exports
        else if List.null imp.items then
          -- Empty import list
          case imp.alias of
            Just _ ->
              -- With alias and no items: qualified access + type aliases for unification
              -- Only add record type aliases, not values (which would conflict)
              -- Use pre-expanded type aliases from exports if available
              let moduleAliases = if Map.isEmpty exports.expandedTypeAliases
                                  then expandModuleAliases exports.typeAliases
                                  else exports.expandedTypeAliases
                  addIfRecordAlias e (Tuple name ty) =
                    if isRecordType ty then extendTypeAlias e name ty else e
              in Array.foldl addIfRecordAlias envWithQualified (Map.toUnfoldable moduleAliases)
            Nothing ->
              -- No alias and no items: qualified + unqualified (e.g., import X)
              mergeExportsWithTypeAliases envWithQualified exports
        else
          -- Import only specified items unqualified (qualified access already added)
          foldl (importItem exports) envWithQualified imp.items

-- | Convert module's TypeAliasInfo map to simple alias map (Map String Type)
-- | Uses module's own aliases to resolve nested references
expandModuleAliases :: Map.Map String TypeAliasInfo -> Map.Map String Type
expandModuleAliases aliasInfos =
  let -- First pass: convert without resolving nested aliases
      initial = Map.mapMaybe (\info ->
        if Array.null (getAliasInfoParams info)
        then Just (typeExprToType Map.empty (getAliasInfoBody info))
        else Nothing) aliasInfos
      -- Second pass: expand any nested references within each alias
      pass2 = Map.mapMaybe (\info ->
        if Array.null (getAliasInfoParams info)
        then Just (typeExprToTypeWithAllAliases initial aliasInfos Map.empty (getAliasInfoBody info))
        else Nothing) aliasInfos
      -- Third pass: expand again using pass2 results for deeper nesting
      pass3 = Map.mapMaybe (\info ->
        if Array.null (getAliasInfoParams info)
        then Just (typeExprToTypeWithAllAliases pass2 aliasInfos Map.empty (getAliasInfoBody info))
        else Nothing) aliasInfos
  in pass3

-- | Import a single item from module exports
importItem :: ModuleExports -> Env -> ImportItem -> Env
importItem exports env item =
  let -- Use pre-expanded type aliases from exports (computed by addValuesToExports)
      -- Fall back to expanding them ourselves for backwards compatibility
      moduleAliases = if Map.isEmpty exports.expandedTypeAliases
                      then expandModuleAliases exports.typeAliases
                      else exports.expandedTypeAliases
      -- Helper to import all RECORD type aliases from a module
      -- This is needed because record type aliases may be referenced by values
      addAllRecordAliases e =
        let addIfRecord e2 (Tuple name ty) =
              if isRecordType ty then extendTypeAlias e2 name ty else e2
        in Array.foldl addIfRecord e (Map.toUnfoldable moduleAliases)
  in case item of
    ImportValue name ->
      -- Import a value or constructor by name
      -- Also check if it's a type alias (type aliases are imported as values in PureScript)
      -- ALSO import all record type aliases from the module (needed for unification)
      case Map.lookup name exports.values of
        Just scheme -> addAllRecordAliases (extendEnv env name scheme)
        Nothing -> case Map.lookup name exports.constructors of
          Just scheme -> addAllRecordAliases (extendEnv env name scheme)
          Nothing ->
            -- Check if it's a type alias
            case Map.lookup name moduleAliases of
              Just ty -> addAllRecordAliases (extendTypeAlias env name ty)
              Nothing -> addAllRecordAliases env  -- Still import record aliases even if value not found
    ImportType typeName spec ->
      -- Import a type and optionally its constructors
      -- First check if it's a type alias and add it to env.typeAliases
      -- Also add any RECORD type aliases that this type references (from the unexpanded TypeExpr)
      let env' = case Map.lookup typeName exports.typeAliases of
            Just aliasInfo ->
              -- Add the expanded type alias itself
              let expandedTy = case Map.lookup typeName moduleAliases of
                    Just ty -> ty
                    Nothing -> typeExprToType Map.empty (getAliasInfoBody aliasInfo)
                  e1 = extendTypeAlias env typeName expandedTy
                  -- Collect type names from the UNEXPANDED body (TypeExpr) to find referenced aliases
                  referencedNames = collectTypeExprNames (getAliasInfoBody aliasInfo)
                  addIfRecordAlias e name = case Map.lookup name moduleAliases of
                    Just aliasTy | isRecordType aliasTy -> extendTypeAlias e name aliasTy
                    _ -> e
              in Array.foldl addIfRecordAlias e1 (Array.fromFoldable referencedNames)
            Nothing ->
              -- Check if it's in moduleAliases (simple type alias from expansion)
              case Map.lookup typeName moduleAliases of
                Just ty -> extendTypeAlias env typeName ty
                Nothing -> env
      in case Map.lookup typeName exports.types of
        Nothing ->
          -- Not a data type, but still import record type aliases from module
          -- (needed for types that reference record aliases internally)
          addAllRecordAliases env'
        Just typeInfo ->
          case spec of
            ImportAll ->
              -- Import all constructors and their record type alias dependencies
              addAllRecordAliases (mergeTypeExport env' exports typeName typeInfo.constructors)
            ImportSome ctorNames ->
              -- Import specific constructors
              addAllRecordAliases (mergeTypeExport env' exports typeName (Array.fromFoldable ctorNames))
            ImportNone ->
              -- Import just the type, no constructors, but still need record aliases
              addAllRecordAliases env'

-- | Resolved import mapping: maps imported names to their source module
-- | This is used by CodeGen to generate qualified calls
type ResolvedImports = Map.Map String String

-- | Collect resolved imports from declarations
-- | Returns a map of imported name -> source module name
collectResolvedImports :: ModuleRegistry -> Array Declaration -> ResolvedImports
collectResolvedImports registry decls =
  Array.foldl collectFromDecl Map.empty decls
  where
    collectFromDecl :: ResolvedImports -> Declaration -> ResolvedImports
    collectFromDecl acc (DeclImport imp) = collectFromImportDecl registry acc imp
    collectFromDecl acc _ = acc

-- | Collect resolved imports from a single import declaration
-- Note: Qualified imports (e.g., "import Data.Set as Set") do NOT add unqualified names
collectFromImportDecl :: ModuleRegistry -> ResolvedImports -> ImportDeclaration -> ResolvedImports
collectFromImportDecl registry acc imp =
  case lookupModule registry imp.moduleName of
    Nothing -> acc  -- Module not found
    Just exports ->
      -- If there's an alias, this is a qualified import - don't add unqualified names
      case imp.alias of
        Just _ ->
          -- Qualified import (e.g., "import Data.Set as Set") - only allow qualified access
          -- If there are explicit items, add those (they can be accessed unqualified)
          if List.null imp.items
          then acc  -- No unqualified imports
          else Array.foldl (\m item -> Map.insert (getImportItemName item) imp.moduleName m) acc (Array.fromFoldable imp.items)
        Nothing ->
          -- Unqualified import
          if imp.hiding then
            -- Import everything except hidden items
            let allNames = getExportedNames exports
                hiddenNames = Set.fromFoldable (map getImportItemName (Array.fromFoldable imp.items))
            in Array.foldl (\m name -> if Set.member name hiddenNames then m else Map.insert name imp.moduleName m) acc allNames
          else if List.null imp.items then
            -- Empty import list - import everything
            let allNames = getExportedNames exports
            in Array.foldl (\m name -> Map.insert name imp.moduleName m) acc allNames
          else
            -- Import only specified items
            Array.foldl (\m item -> Map.insert (getImportItemName item) imp.moduleName m) acc (Array.fromFoldable imp.items)

-- | Get all exported names from a module's exports
getExportedNames :: ModuleExports -> Array String
getExportedNames exports =
  let valueNames = map (\(Tuple k _) -> k) (Map.toUnfoldable exports.values)
      ctorNames = map (\(Tuple k _) -> k) (Map.toUnfoldable exports.constructors)
  in valueNames <> ctorNames

-- | Get the name from an ImportItem
getImportItemName :: ImportItem -> String
getImportItemName (ImportValue name) = name
getImportItemName (ImportType name _) = name

-- | Collect type aliases from all imported modules
-- Returns a map of alias name to TypeAliasInfo
collectImportedAliases :: ModuleRegistry -> Array Declaration -> Map.Map String TypeAliasInfo
collectImportedAliases registry decls =
  Array.foldl collectFromImport Map.empty decls
  where
    collectFromImport acc (DeclImport imp) =
      case lookupModule registry imp.moduleName of
        Nothing -> acc
        Just exports ->
          let aliases = exports.typeAliases
              -- Add unqualified aliases
              acc1 = Map.union acc aliases
              -- Add qualified aliases with the import prefix
              prefix = case imp.alias of
                Just alias -> alias
                Nothing ->
                  -- Use last component of module name if no alias
                  case String.lastIndexOf (String.Pattern ".") imp.moduleName of
                    Nothing -> imp.moduleName
                    Just idx -> String.drop (idx + 1) imp.moduleName
              -- Add aliases with prefix, e.g., "Cst.SourcePos"
              addWithPrefix m (Tuple name info) = Map.insert (prefix <> "." <> name) info m
              acc2 = Array.foldl addWithPrefix acc1 (Map.toUnfoldable aliases)
          in acc2
    collectFromImport acc _ = acc

-- | Extract exports from a module's declarations
-- This is used to build the registry from parsed modules
extractExports :: Array Declaration -> ModuleExports
extractExports decls =
  Array.foldl collectExport emptyExports decls
  where
    collectExport :: ModuleExports -> Declaration -> ModuleExports
    collectExport exp (DeclDataType dt) =
      let -- Add the type
          typeInfo = { arity: List.length dt.typeVars
                     , constructors: Array.fromFoldable (map _.name dt.constructors)
                     }
          exp1 = exp { types = Map.insert dt.name typeInfo exp.types }
          -- Add constructors (we need to compute their types)
          -- For now, just store the constructor names - actual schemes would need type checking
      in Array.foldl (addConstructorPlaceholder dt) exp1 (Array.fromFoldable dt.constructors)
    collectExport exp (DeclTypeAlias ta) =
      exp { typeAliases = Map.insert ta.name { params: Array.fromFoldable ta.typeVars, body: ta.ty } exp.typeAliases }
    collectExport exp (DeclNewtype nt) =
      -- Newtypes export both a type and its constructor
      let typeInfo = { arity: List.length nt.typeVars
                     , constructors: [nt.constructor]
                     }
          exp1 = exp { types = Map.insert nt.name typeInfo exp.types }
          -- Build constructor type: wrappedType -> NewtypeName a b ...
          typeVarPairs = Array.mapWithIndex (\i v -> Tuple v (mkTVar i v)) (Array.fromFoldable nt.typeVars)
          typeVarMap = Map.fromFoldable typeVarPairs
          resultType = if List.null nt.typeVars
                       then TyCon (mkTCon nt.name [])
                       else TyCon (mkTCon nt.name (map (\(Tuple _ tv) -> TyVar tv) typeVarPairs))
          wrappedTy = typeExprToTypeWithAllAliases aliasMap paramAliasMap typeVarMap nt.wrappedType
          ctorType = tArrow wrappedTy resultType
          scheme = mkScheme (map snd typeVarPairs) ctorType
      in exp1 { constructors = Map.insert nt.constructor scheme exp1.constructors }
    collectExport exp (DeclFunction func) =
      -- Functions need their types inferred, so we can't add them pre-typecheck
      -- They would be added after type checking
      exp
    collectExport exp (DeclForeignImport fi) =
      -- Foreign imports have explicit type signatures, so we can add them
      let ty = typeExprToType Map.empty fi.typeSignature
          freeVars = Array.fromFoldable (Set.toUnfoldable (freeTypeVars ty) :: Array Int)
          tvars = map (\id -> { id: id, name: "a" <> intToString id }) freeVars
          scheme = mkScheme tvars ty
      in exp { values = Map.insert fi.functionName scheme exp.values }
    collectExport exp (DeclTypeClass tc) =
      -- Add type class methods to exports with their polymorphic types
      Array.foldl addMethod exp (Array.fromFoldable tc.methods)
      where
        addMethod e sig =
          let varPairs = Array.mapWithIndex (\i v -> Tuple v (mkTVar i v)) (Array.fromFoldable tc.typeVars)
              varMap = Map.fromFoldable varPairs
              methodType = typeExprToType varMap sig.ty
              scheme = mkScheme (map snd varPairs) methodType
          in e { values = Map.insert sig.name scheme e.values }
    collectExport exp _ = exp

    -- Build alias maps for resolving field types
    aliasMap = collectTypeAliases decls
    paramAliasMap = collectParamTypeAliases decls

    addConstructorPlaceholder :: DataType -> ModuleExports -> DataConstructor -> ModuleExports
    addConstructorPlaceholder dt exp ctor =
      -- Build proper constructor type using type aliases
      let -- Create type var map for the data type's type parameters
          typeVarPairs = Array.mapWithIndex (\i v -> Tuple v (mkTVar i v)) (Array.fromFoldable dt.typeVars)
          typeVarMap = Map.fromFoldable typeVarPairs
          -- Result type is the data type applied to its type vars
          resultType = if List.null dt.typeVars
                       then TyCon (mkTCon dt.name [])
                       else TyCon (mkTCon dt.name (map (\(Tuple _ tv) -> TyVar tv) typeVarPairs))
          -- Build constructor type: field1 -> field2 -> ... -> ResultType
          -- Use buildConstructorTypeWithAllAliases to fully expand nested type aliases
          ctorType = buildConstructorTypeWithAllAliases aliasMap paramAliasMap typeVarMap ctor.fields resultType
          scheme = mkScheme (map snd typeVarPairs) ctorType
      in exp { constructors = Map.insert ctor.name scheme exp.constructors }

-- | Add values to exports after type checking (when we know the types)
-- | Also computes expanded type aliases for unification
addValuesToExports :: ModuleExports -> Env -> Array Declaration -> ModuleExports
addValuesToExports exports env decls =
  let -- First add function values
      withValues = Array.foldl addValue exports decls
      -- Then compute expanded type aliases from this module's declarations only
      -- Note: We only export local type aliases, not imported ones
      expandedAliases = collectTypeAliases decls
  in withValues { expandedTypeAliases = expandedAliases }
  where
    addValue exp (DeclFunction func) =
      case Map.lookup func.name env.bindings of
        Just scheme -> exp { values = Map.insert func.name scheme exp.values }
        Nothing -> exp
    addValue exp _ = exp

-- | Collect type aliases from declarations into a Map String Type
-- | For non-parameterized aliases only (backward compatible)
-- | Uses two-pass approach: first collect all alias info, then expand with full context
collectTypeAliases :: Array Declaration -> Map.Map String Type
collectTypeAliases decls = collectTypeAliasesWithBase Map.empty (collectParamTypeAliases decls) decls

-- | Collect type aliases with a base alias map for resolving references
-- | Two-pass approach: first build initial map without expansion, then expand with full context
collectTypeAliasesWithBase :: Map.Map String Type -> Map.Map String TypeAliasInfo -> Array Declaration -> Map.Map String Type
collectTypeAliasesWithBase baseAliases paramAliases decls =
  let -- First pass: build initial map with no expansion (just basic conversion)
      initial = Array.foldl collectInitial baseAliases decls
      -- Second pass: expand all aliases using the complete initial map
      expanded = Array.foldl (collectExpanded initial) baseAliases decls
  in expanded
  where
    collectInitial m decl = case decl of
      DeclTypeAlias ta ->
        if List.null ta.typeVars
        then Map.insert ta.name (typeExprToType Map.empty ta.ty) m
        else m  -- Skip parameterized aliases here
      _ -> m
    collectExpanded initialMap m decl = case decl of
      DeclTypeAlias ta ->
        if List.null ta.typeVars
        then Map.insert ta.name (typeExprToTypeWithAllAliases initialMap paramAliases Map.empty ta.ty) m
        else m  -- Skip parameterized aliases here
      _ -> m

-- | Collect parameterized type aliases into a Map String TypeAliasInfo
collectParamTypeAliases :: Array Declaration -> Map.Map String TypeAliasInfo
collectParamTypeAliases decls = Array.foldl collect Map.empty decls
  where
    collect m (DeclTypeAlias ta) = Map.insert ta.name { params: Array.fromFoldable ta.typeVars, body: ta.ty } m
    collect m _ = m

-- | Process non-function declarations (data types, type aliases, imports, type sigs)
-- Process in two phases: type aliases first, then data types
-- This ensures type aliases are available when processing data constructor fields
processNonFunctions :: Env -> Array Declaration -> Env
processNonFunctions env decls = processNonFunctionsWithAliases Map.empty env decls

-- | Process non-function declarations with imported type aliases
processNonFunctionsWithAliases :: Map.Map String TypeAliasInfo -> Env -> Array Declaration -> Env
processNonFunctionsWithAliases importedAliases env decls =
  let -- Phase 1: Collect parameterized aliases (local + imported)
      localParamAliasMap = collectParamTypeAliases decls
      paramAliasMap = Map.union localParamAliasMap importedAliases
      -- Convert imported TypeAliasInfo to Type for simple aliases
      importedSimpleAliases = Map.mapMaybe (\info ->
        if Array.null (getAliasInfoParams info)
        then Just (typeExprToType Map.empty (getAliasInfoBody info))
        else Nothing) importedAliases
      -- Collect local aliases with imported as base for resolving references
      localAliasMap = collectTypeAliasesWithBase importedSimpleAliases paramAliasMap decls
      aliasMap = localAliasMap  -- Already includes importedSimpleAliases from base
      -- Also add aliases to env for other purposes
      processTypeAlias e decl = case decl of
        DeclTypeAlias ta -> checkTypeAlias e ta
        _ -> e
      env1 = Array.foldl processTypeAlias env decls
      -- Phase 2: Process data types and newtypes (with both simple and parameterized type aliases)
      processDataType e decl = case decl of
        DeclDataType dt -> checkDataTypeWithAllAliases aliasMap paramAliasMap e dt
        DeclNewtype nt -> checkNewtypeWithAllAliases aliasMap paramAliasMap e nt
        _ -> e
      env2 = Array.foldl processDataType env1 decls
      -- Phase 3: Process type classes (add methods to env)
      processTypeClass e decl = case decl of
        DeclTypeClass tc -> checkTypeClass e tc
        _ -> e
      env3 = Array.foldl processTypeClass env2 decls
  in env3

-- | Process infix declarations - register operators as aliases for functions
-- | This must be called after addFunctionPlaceholders so function types are available
processInfixDeclarations :: Env -> Array Declaration -> Env
processInfixDeclarations env decls =
  Array.foldl processInfix env decls
  where
    processInfix e (DeclInfix inf) =
      -- Look up the type of the function and register the operator with the same type
      case lookupEnv e inf.functionName of
        Just scheme -> extendEnv e inf.operator scheme
        Nothing -> e  -- Function not found, skip
    processInfix e _ = e

-- | Add placeholder types for all functions
-- Uses type signature if available, otherwise creates a fresh type variable
addFunctionPlaceholders :: Env -> Array Declaration -> Env
addFunctionPlaceholders env decls = addFunctionPlaceholdersWithAliases Map.empty env decls

-- | Add placeholder types for all functions with imported type aliases
addFunctionPlaceholdersWithAliases :: Map.Map String TypeAliasInfo -> Env -> Array Declaration -> Env
addFunctionPlaceholdersWithAliases importedAliases env decls =
  let -- First collect parameterized aliases (local + imported)
      localParamAliasMap = collectParamTypeAliases decls
      paramAliasMap = Map.union localParamAliasMap importedAliases
      -- Convert imported TypeAliasInfo to Type for simple aliases
      importedSimpleAliases = Map.mapMaybe (\info ->
        if Array.null (getAliasInfoParams info)
        then Just (typeExprToType Map.empty (getAliasInfoBody info))
        else Nothing) importedAliases
      -- Collect local type aliases with imported aliases as base for resolving references
      localAliasMap = collectTypeAliasesWithBase importedSimpleAliases paramAliasMap decls
      -- Final alias map includes both imported and locally expanded aliases
      aliasMap = localAliasMap  -- Already includes importedSimpleAliases from base
      -- Collect standalone type signatures into a map
      collectSig m decl = case decl of
        DeclTypeSig sig -> Map.insert sig.name sig.ty m
        _ -> m
      sigMap = Array.foldl collectSig Map.empty decls
      -- Add placeholders for all functions
      addPlaceholder e decl = case decl of
        DeclFunction func ->
          -- First check embedded type signature in the function (func.typeSignature is Maybe TypeSignature)
          case func.typeSignature of
            Just sig ->
              -- sig is a TypeSignature record with { name, typeVars, constraints, ty }
              let ty = typeExprToTypeWithAllAliases aliasMap paramAliasMap Map.empty sig.ty
                  scheme = mkScheme [] ty
              in extendEnv e func.name scheme
            Nothing ->
              -- Then check standalone signatures
              case Map.lookup func.name sigMap of
                Just tyExpr ->
                  let ty = typeExprToTypeWithAllAliases aliasMap paramAliasMap Map.empty tyExpr
                      scheme = mkScheme [] ty
                  in extendEnv e func.name scheme
                Nothing ->
                  -- No signature, add fresh type variable
                  let Tuple tv e' = freshVar e ("fn_" <> func.name)
                  in extendEnv e' func.name (mkScheme [] (TyVar tv))
        DeclForeignImport fi ->
          -- Foreign imports have explicit type signatures
          let ty = typeExprToTypeWithAllAliases aliasMap paramAliasMap Map.empty fi.typeSignature
              freeVars = Array.fromFoldable (Set.toUnfoldable (freeTypeVars ty) :: Array Int)
              tvars = map (\id -> { id: id, name: "a" <> intToString id }) freeVars
              scheme = mkScheme tvars ty
          in extendEnv e fi.functionName scheme
        _ -> e
  in Array.foldl addPlaceholder env decls

-- | Helper for checkFunctionBodies
checkFunctionBodiesGo :: Env -> Array Declaration -> Either TCError Env
checkFunctionBodiesGo e ds = case Array.uncons ds of
  Nothing -> Right e
  Just { head: DeclFunction func, tail: rest } ->
    case checkFunction e func of
      Left err -> Left (addErrorContext ("function '" <> func.name <> "'") err)
      Right r -> checkFunctionBodiesGo r.env rest
  Just { head: _, tail: rest } -> checkFunctionBodiesGo e rest

-- | Type check all function bodies
-- Handles multi-clause functions by merging adjacent declarations with the same name
checkFunctionBodies :: Env -> Array Declaration -> Either TCError Env
checkFunctionBodies env decls =
  let mergedDecls = mergeMultiClauseFunctions decls
  in checkFunctionBodiesGo env mergedDecls

-- | Merge adjacent function declarations with the same name into single functions
-- Converts: f (Pat1) = body1; f (Pat2) = body2
-- Into:     f x = case x of Pat1 -> body1; Pat2 -> body2
mergeMultiClauseFunctions :: Array Declaration -> Array Declaration
mergeMultiClauseFunctions decls = goMerge decls []
  where
    goMerge ds acc = case Array.uncons ds of
      Nothing -> Array.reverse acc
      Just { head: d, tail: rest } -> case d of
        DeclFunction func ->
          -- Collect all adjacent functions with the same name
          let { sameName, remaining } = collectSameName func.name rest []
              allClauses = Array.cons func sameName
          in if Array.length allClauses > 1
             then goMerge remaining (Array.cons (DeclFunction (mergeClausesIntoOne allClauses)) acc)
             else goMerge rest (Array.cons d acc)
        _ -> goMerge rest (Array.cons d acc)

    -- Collect adjacent DeclFunction with same name (using arrays)
    collectSameName :: String -> Array Declaration -> Array FunctionDeclaration -> { sameName :: Array FunctionDeclaration, remaining :: Array Declaration }
    collectSameName name ds acc = case Array.uncons ds of
      Nothing -> { sameName: acc, remaining: [] }
      Just { head: DeclFunction f, tail: rest } ->
        if f.name == name
        then collectSameName name rest (Array.snoc acc f)
        else { sameName: acc, remaining: ds }
      Just _ -> { sameName: acc, remaining: ds }

    -- Merge multiple function clauses into one with case expression
    mergeClausesIntoOne :: Array FunctionDeclaration -> FunctionDeclaration
    mergeClausesIntoOne clauses =
      case Array.head clauses of
        Nothing -> { name: "", parameters: Nil, body: ExprVar "error", guards: Nil, typeSignature: Nothing }
        Just first ->
          let name = first.name
              -- All clauses should have same number of parameters
              numParams = List.length first.parameters
              -- Create fresh parameter names
              paramNames = map (\i -> "__arg" <> intToString i) (List.range 0 (numParams - 1))
              paramPats = map PatVar paramNames
              paramVars = map ExprVar paramNames
              -- Build case clauses from each function clause
              clausesList = List.fromFoldable clauses
              caseClauses = List.mapMaybe (clauseToCaseClause paramVars) clausesList
              -- Build case expression or tuple case for multiple params
              caseExpr = case numParams of
                0 -> first.body  -- No params, just use first body
                1 -> case List.head paramVars of
                  Just v -> ExprCase v caseClauses
                  Nothing -> first.body
                _ -> ExprCase (ExprTuple paramVars) (List.mapMaybe (clauseToTupleCase paramVars) clausesList)
          in { name
             , parameters: paramPats
             , body: caseExpr
             , guards: Nil
             , typeSignature: first.typeSignature
             }

    -- Convert a function clause to a case clause (single parameter case)
    clauseToCaseClause :: List Expr -> FunctionDeclaration -> Maybe CaseClause
    clauseToCaseClause _ func = case List.head func.parameters of
      Nothing -> Nothing
      Just pat -> Just { pattern: pat, body: func.body, guard: Nothing }

    -- Convert a function clause to a case clause with tuple pattern (multi-param)
    clauseToTupleCase :: List Expr -> FunctionDeclaration -> Maybe CaseClause
    clauseToTupleCase _ func =
      let n = List.length func.parameters
          -- Use "Tuple" for 2 params, "Tuple3" for 3, etc. to match PureScript conventions
          tupName = if n == 2 then "Tuple" else "Tuple" <> intToString n
      in if n > 1
         then Just { pattern: PatCon tupName func.parameters, body: func.body, guard: Nothing }
         else clauseToCaseClause Nil func
