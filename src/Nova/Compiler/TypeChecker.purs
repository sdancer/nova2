module Nova.Compiler.TypeChecker where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Data.Array ((:))
import Data.Array as Array
import Data.List (List(..))
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Nova.Compiler.Types (Type(..), TVar, Scheme, Env, Subst, emptySubst, composeSubst, applySubst, applySubstToEnv, freeTypeVars, freeTypeVarsEnv, freshVar, extendEnv, lookupEnv, mkScheme, mkTVar, mkTCon, tInt, tString, tChar, tBool, tArrow, tArray, tTuple)
import Nova.Compiler.Ast (Expr(..), Literal(..), Pattern(..), LetBind, CaseClause, Declaration(..), FunctionDeclaration, DoStatement(..), DataType, DataConstructor, DataField, TypeExpr(..), TypeAlias)
import Nova.Compiler.Unify (UnifyError, unify)

-- | Type alias info for parameterized type aliases
-- | e.g., type ParseResult a = Either String (Tuple a (Array Token))
-- | has params = ["a"], body = TyExprApp ...
type TypeAliasInfo = { params :: Array String, body :: TypeExpr }

-- | Type checking error
data TCError
  = UnifyErr UnifyError
  | UnboundVariable String
  | NotImplemented String

instance showTCError :: Show TCError where
  show (UnifyErr e) = "Unification error: " <> show e
  show (UnboundVariable v) = "Unbound variable: " <> v
  show (NotImplemented s) = "Not implemented: " <> s

-- | Instantiate a type scheme with fresh variables
instantiate :: Env -> Scheme -> { ty :: Type, env :: Env }
instantiate env scheme = go env scheme.vars Map.empty
  where
    go e [] sub = { ty: applySubst sub scheme.ty, env: e }
    go e vars sub = case Array.uncons vars of
      Nothing -> { ty: applySubst sub scheme.ty, env: e }
      Just { head: v, tail: rest } ->
        let Tuple fresh e' = freshVar e v.name
            sub' = Map.insert v.id (TyVar fresh) sub
        in go e' rest sub'

-- | Generalize a type to a scheme
generalize :: Env -> Type -> Scheme
generalize env ty =
  let envFree = freeTypeVarsEnv env
      tyFree = freeTypeVars ty
      freeIds = Set.toUnfoldable (Set.difference tyFree envFree)
      vars = map (\i -> mkTVar i ("t" <> show i)) freeIds
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
        Nothing -> Left (UnboundVariable fullName)

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
              in case unify (applySubst r2.sub r1.ty) (tArrow r2.ty resultTy) of
                Left ue -> Left (UnifyErr ue)
                Right s3 ->
                  let sub = composeSubst s3 (composeSubst r2.sub r1.sub)
                  in Right { ty: applySubst s3 resultTy, sub, env: env3 }

infer env (ExprLambda pats body) =
  case Array.uncons pats of
    Nothing -> infer env body
    Just { head: pat, tail: restPats } ->
      let Tuple argTv env1 = freshVar env "a"
          argTy = TyVar argTv
      in case inferPat env1 pat argTy of
        Left e -> Left e
        Right patRes ->
          let innerExpr = if Array.null restPats then body else ExprLambda restPats body
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
      case unify condRes.ty tBool of
        Left ue -> Left (UnifyErr ue)
        Right _ ->
          case infer condRes.env then_ of
            Left e -> Left e
            Right thenRes ->
              case infer thenRes.env else_ of
                Left e -> Left e
                Right elseRes ->
                  case unify (applySubst elseRes.sub thenRes.ty) elseRes.ty of
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
  case lookupEnv env op of
    Nothing -> Left (UnboundVariable op)
    Just scheme ->
      let opInst = instantiate env scheme
      in case infer opInst.env l of
        Left e -> Left e
        Right lRes ->
          case infer lRes.env r of
            Left e -> Left e
            Right rRes ->
              let Tuple resTv env4 = freshVar rRes.env "binop"
              in case unify (applySubst (composeSubst rRes.sub lRes.sub) opInst.ty) (tArrow lRes.ty (tArrow rRes.ty (TyVar resTv))) of
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
      in case unify recRes.ty expectedRec of
        Left ue -> Left (UnifyErr ue)
        Right s2 ->
          let sub = composeSubst s2 recRes.sub
          in Right { ty: applySubst sub (TyVar resultTv), sub, env: env3 }

infer env (ExprParens e) = infer env e

infer env (ExprTyped e _) = infer env e

infer env (ExprRecordUpdate rec updates) = inferRecordUpdate env rec updates

infer env (ExprUnaryOp op e) = inferUnaryOp env op e

infer env (ExprDo stmts) = inferDo env stmts

infer _ _ = Left (NotImplemented "expression form")

-- | Infer record update: rec { field = value, ... }
inferRecordUpdate :: Env -> Expr -> Array (Tuple String Expr) -> Either TCError InferResult
inferRecordUpdate env rec updates = do
  -- Infer the base record type
  recRes <- infer env rec
  -- Infer each update field
  updateRes <- inferFields recRes.env updates
  -- The result type is the record with updated fields
  -- For now, we just unify to ensure fields exist
  let Tuple rowVar env2 = freshVar updateRes.env "row"
      updateFields = Map.fromFoldable updateRes.tys
      expectedRec = TyRecord { fields: updateFields, row: Just rowVar }
  case unify (applySubst updateRes.sub recRes.ty) expectedRec of
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
      case unify res.ty tInt of
        Left ue -> Left (UnifyErr ue)
        Right s -> Right { ty: tInt, sub: composeSubst s res.sub, env: res.env }
    "!" -> do
      -- Boolean negation: Bool -> Bool
      res <- infer env e
      case unify res.ty tBool of
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
          case unify opInst.ty (tArrow res.ty (TyVar resTv)) of
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

inferMany :: Env -> Array Expr -> Either TCError ManyResult
inferMany env exprs = go env exprs [] emptySubst
  where
    go e [] acc sub = Right { tys: Array.reverse acc, sub, env: e }
    go e es acc sub = case Array.uncons es of
      Nothing -> Right { tys: Array.reverse acc, sub, env: e }
      Just { head: expr, tail: rest } ->
        case infer e expr of
          Left err -> Left err
          Right res -> go res.env rest (res.ty : acc) (composeSubst res.sub sub)

-- | Infer list element types
type ElemsResult = { sub :: Subst, env :: Env }

inferElems :: Env -> Type -> Array Expr -> Either TCError ElemsResult
inferElems env elemTy elems = go env elemTy elems emptySubst
  where
    go e _ [] sub = Right { sub, env: e }
    go e eTy es sub = case Array.uncons es of
      Nothing -> Right { sub, env: e }
      Just { head: expr, tail: rest } ->
        case infer e expr of
          Left err -> Left err
          Right res ->
            case unify (applySubst res.sub eTy) res.ty of
              Left ue -> Left (UnifyErr ue)
              Right s2 -> go res.env (applySubst s2 eTy) rest (composeSubst s2 (composeSubst res.sub sub))

-- | Infer record field types
type FieldsResult = { tys :: Array (Tuple String Type), sub :: Subst, env :: Env }

inferFields :: Env -> Array (Tuple String Expr) -> Either TCError FieldsResult
inferFields env fields = go env fields [] emptySubst
  where
    go e [] acc sub = Right { tys: Array.reverse acc, sub, env: e }
    go e fs acc sub = case Array.uncons fs of
      Nothing -> Right { tys: Array.reverse acc, sub, env: e }
      Just { head: Tuple name expr, tail: rest } ->
        case infer e expr of
          Left err -> Left err
          Right res -> go res.env rest (Tuple name res.ty : acc) (composeSubst res.sub sub)

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
  in case unify ty litTy of
    Left ue -> Left (UnifyErr ue)
    Right s -> Right { env, sub: s }

inferPat env (PatCon conName pats) ty =
  case lookupEnv env conName of
    Nothing -> Left (UnboundVariable conName)
    Just scheme ->
      let r = instantiate env scheme
      in inferConPats r.env r.ty pats ty

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
inferRecordPat :: Env -> Array (Tuple String Pattern) -> Type -> Either TCError PatResult
inferRecordPat env fields ty = go env fields Map.empty emptySubst
  where
    go e [] fieldTypes sub = do
      -- Build expected record type and unify
      let Tuple rowVar e' = freshVar e "row"
          expectedRec = TyRecord { fields: fieldTypes, row: Just rowVar }
      case unify (applySubst sub ty) expectedRec of
        Left ue -> Left (UnifyErr ue)
        Right s -> Right { env: e', sub: composeSubst s sub }
    go e fs fieldTypes sub = case Array.uncons fs of
      Nothing -> go e [] fieldTypes sub
      Just { head: Tuple label pat, tail: rest } -> do
        -- Create fresh type for this field
        let Tuple fieldVar e1 = freshVar e ("f_" <> label)
            fieldTy = TyVar fieldVar
        -- Infer the pattern with this field type
        patRes <- inferPat e1 pat fieldTy
        -- Continue with remaining fields
        go patRes.env rest (Map.insert label (applySubst patRes.sub fieldTy) fieldTypes) (composeSubst patRes.sub sub)

-- | Infer list pattern: [a, b, c]
inferListPat :: Env -> Array Pattern -> Type -> Either TCError PatResult
inferListPat env pats ty = do
  -- Create fresh element type
  let Tuple elemVar env1 = freshVar env "elem"
      elemTy = TyVar elemVar
  -- Unify ty with Array elemTy
  case unify ty (tArray elemTy) of
    Left ue -> Left (UnifyErr ue)
    Right s1 -> do
      -- Infer each element pattern with the element type
      let elemTy' = applySubst s1 elemTy
      goElems env1 pats elemTy' s1
  where
    goElems e [] _ sub = Right { env: e, sub }
    goElems e ps eTy sub = case Array.uncons ps of
      Nothing -> Right { env: e, sub }
      Just { head: p, tail: rest } -> do
        patRes <- inferPat e p eTy
        goElems patRes.env rest (applySubst patRes.sub eTy) (composeSubst patRes.sub sub)

-- | Infer cons pattern: (h : t)
inferConsPat :: Env -> Pattern -> Pattern -> Type -> Either TCError PatResult
inferConsPat env hdPat tlPat ty = do
  -- Create fresh element type
  let Tuple elemVar env1 = freshVar env "elem"
      elemTy = TyVar elemVar
  -- Unify ty with Array elemTy
  case unify ty (tArray elemTy) of
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
inferConPats :: Env -> Type -> Array Pattern -> Type -> Either TCError PatResult
inferConPats env conTy pats resultTy = go env conTy pats emptySubst
  where
    go e ty [] sub =
      case unify ty resultTy of
        Left ue -> Left (UnifyErr ue)
        Right s -> Right { env: e, sub: composeSubst s sub }
    go e ty ps sub = case Array.uncons ps of
      Nothing -> go e ty [] sub
      Just { head: p, tail: rest } ->
        case ty of
          TyCon c | c.name == "Fun", Array.length c.args == 2 ->
            case { a: Array.head c.args, b: Array.last c.args } of
              { a: Just argTy, b: Just resTy } ->
                case inferPat e p argTy of
                  Left err -> Left err
                  Right patRes -> go patRes.env resTy rest (composeSubst patRes.sub sub)
              _ -> Left (NotImplemented "malformed function type")
          _ -> Left (NotImplemented "expected function type in constructor")

-- | Infer let bindings
inferBinds :: Env -> Array LetBind -> Either TCError PatResult
inferBinds env binds =
  -- First pass: add all bindings with fresh type variables (for recursive refs)
  let envWithPlaceholders = addBindPlaceholders env binds
  -- Second pass: infer actual types
  in inferBindsPass2 envWithPlaceholders binds emptySubst
  where
    addBindPlaceholders :: Env -> Array LetBind -> Env
    addBindPlaceholders e bs = Array.foldl addOne e bs

    addOne :: Env -> LetBind -> Env
    addOne e bind = case bind.pattern of
      PatVar name ->
        let Tuple tv e' = freshVar e ("let_" <> name)
        in extendEnv e' name (mkScheme [] (TyVar tv))
      _ -> e

    inferBindsPass2 :: Env -> Array LetBind -> Subst -> Either TCError PatResult
    inferBindsPass2 e [] sub = Right { env: e, sub }
    inferBindsPass2 e bs sub = case Array.uncons bs of
      Nothing -> Right { env: e, sub }
      Just { head: bind, tail: rest } ->
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

-- | Infer case clauses
inferClauses :: Env -> Type -> Type -> Array CaseClause -> Subst -> Either TCError PatResult
inferClauses env scrutTy resultTy clauses initSub = go env scrutTy resultTy clauses initSub
  where
    go e _ _ [] sub = Right { env: e, sub }
    go e sTy rTy cs sub = case Array.uncons cs of
      Nothing -> Right { env: e, sub }
      Just { head: clause, tail: rest } ->
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
                    case unify (applySubst bodyRes.sub rTy') bodyRes.ty of
                      Left ue -> Left (UnifyErr ue)
                      Right s ->
                        let newSub = composeSubst s (composeSubst bodyRes.sub (composeSubst guardRes.sub (composeSubst patRes.sub sub)))
                            -- IMPORTANT: Update counter from bodyRes.env to avoid type variable ID collisions
                            -- We keep original bindings but use the new counter
                            e' = e { counter = bodyRes.env.counter }
                        in go e' sTy rTy rest newSub

    -- Infer a guard expression (if present)
    -- Pattern guards like `Pat <- Expr` need special handling to bind variables
    inferGuard :: Env -> Maybe Expr -> Either TCError { env :: Env, sub :: Subst }
    inferGuard e Nothing = Right { env: e, sub: emptySubst }
    inferGuard e (Just guardExpr) = inferGuardExpr e guardExpr

    -- Handle pattern guard expressions recursively
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

    -- Convert an expression that's being used as a pattern to an actual Pattern
    exprToPattern :: Expr -> Pattern
    exprToPattern (ExprVar name) = PatVar name
    exprToPattern (ExprApp (ExprVar con) arg) = PatCon con [exprToPattern arg]
    exprToPattern (ExprApp (ExprApp (ExprVar con) arg1) arg2) = PatCon con [exprToPattern arg1, exprToPattern arg2]
    exprToPattern (ExprLit lit) = PatLit lit
    exprToPattern (ExprParens e) = exprToPattern e
    exprToPattern _ = PatWildcard

-- | Type check a function declaration
-- For recursive functions, we first add a fresh type var for the function name
checkFunction :: Env -> FunctionDeclaration -> Either TCError { scheme :: Scheme, env :: Env }
checkFunction env func =
  -- First, add function name with fresh type variable (for recursion)
  let Tuple funcTv env1 = freshVar env ("fn_" <> func.name)
      funcTy = TyVar funcTv
      tempScheme = mkScheme [] funcTy
      envWithFunc = extendEnv env1 func.name tempScheme
      -- Build expression from parameters and body
      expr = if Array.null func.parameters
             then func.body
             else ExprLambda func.parameters func.body
  in case infer envWithFunc expr of
    Left e -> Left e
    Right res -> do
      -- Unify inferred type with the placeholder
      case unify (applySubst res.sub funcTy) res.ty of
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

checkDecl env _ = Right env

-- | Process a data type declaration
-- | Adds the type constructor and all data constructors to the environment
checkDataType :: Env -> DataType -> Env
checkDataType env dt = checkDataTypeWithAliases Map.empty env dt

-- | Process a data type declaration with access to type aliases
-- | aliasMap is Map String Type containing type alias expansions
checkDataTypeWithAliases :: Map.Map String Type -> Env -> DataType -> Env
checkDataTypeWithAliases aliasMap env dt =
  let -- Create type variables for the type parameters
      typeVarPairs = Array.mapWithIndex (\i v -> Tuple v (mkTVar (env.counter + i) v)) dt.typeVars
      typeVarMap = Map.fromFoldable typeVarPairs
      newCounter = env.counter + Array.length dt.typeVars
      env1 = env { counter = newCounter }

      -- The result type is the data type applied to its type variables
      typeArgs = map (\(Tuple _ tv) -> TyVar tv) typeVarPairs
      resultType = TyCon (mkTCon dt.name typeArgs)

      -- Add each constructor to the environment
      addConstructor e con =
        let conType = buildConstructorTypeWithAliases aliasMap typeVarMap con.fields resultType
            conScheme = mkScheme (map snd typeVarPairs) conType
        in extendEnv e con.name conScheme
  in Array.foldl addConstructor env1 dt.constructors

-- | Build the type for a data constructor
-- | e.g., Just :: forall a. a -> Maybe a
-- | e.g., Cons :: forall a. a -> List a -> List a
buildConstructorType :: Map.Map String TVar -> Array DataField -> Type -> Type
buildConstructorType varMap fields resultType = go fields
  where
    go [] = resultType
    go fs = case Array.uncons fs of
      Nothing -> resultType
      Just { head: field, tail: rest } ->
        let fieldTy = typeExprToType varMap field.ty
        in tArrow fieldTy (go rest)

-- | Build constructor type with type alias lookup
buildConstructorTypeWithAliases :: Map.Map String Type -> Map.Map String TVar -> Array DataField -> Type -> Type
buildConstructorTypeWithAliases aliasMap varMap fields resultType = go fields
  where
    go [] = resultType
    go fs = case Array.uncons fs of
      Nothing -> resultType
      Just { head: field, tail: rest } ->
        let fieldTy = typeExprToTypeWithAliases aliasMap varMap field.ty
        in tArrow fieldTy (go rest)

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
      tryLookup nm = case Map.lookup nm aliasMap of
        Just ty -> Just ty
        Nothing -> case Map.lookup nm paramAliasMap of
          Just info | Array.null info.params ->
            Just (typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap info.body)
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
        Just info | Array.length info.params == Array.length args ->
          -- Found matching parameterized alias, substitute params
          let argTypes = map (typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap) args
              paramSubst = Map.fromFoldable (Array.zip info.params argTypes)
          in substituteTypeExpr aliasMap paramAliasMap paramSubst info.body
        _ ->
          -- Not a parameterized alias or wrong arity, normal type application
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
  let varList = Array.mapWithIndex (\i name -> Tuple name (mkTVar (-(i + 1)) name)) vars
      newVarMap = Map.union (Map.fromFoldable varList) varMap
  in typeExprToTypeWithAllAliases aliasMap paramAliasMap newVarMap t
typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap (TyExprConstrained _ t) = typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap t
typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap (TyExprParens t) = typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap t
typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap (TyExprTuple ts) =
  tTuple (map (typeExprToTypeWithAllAliases aliasMap paramAliasMap varMap) ts)

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
      tryLookup nm = case Map.lookup nm aliasMap of
        Just ty -> Just ty
        Nothing -> case Map.lookup nm paramAliasMap of
          Just info | Array.null info.params ->
            Just (typeExprToTypeWithEnv env aliasMap paramAliasMap varMap info.body)
          _ -> Nothing
      -- Check if it's a type alias in the environment (from imported modules)
      tryEnvLookup nm = case lookupEnv env nm of
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
        Just info | Array.length info.params == Array.length args ->
          -- Found matching parameterized alias, substitute params
          let argTypes = map (typeExprToTypeWithEnv env aliasMap paramAliasMap varMap) args
              paramSubst = Map.fromFoldable (Array.zip info.params argTypes)
          in substituteTypeExpr aliasMap paramAliasMap paramSubst info.body
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
  let varList = Array.mapWithIndex (\i name -> Tuple name (mkTVar (-(i + 1)) name)) vars
      newVarMap = Map.union (Map.fromFoldable varList) varMap
  in typeExprToTypeWithEnv env aliasMap paramAliasMap newVarMap t
typeExprToTypeWithEnv env aliasMap paramAliasMap varMap (TyExprConstrained _ t) =
  typeExprToTypeWithEnv env aliasMap paramAliasMap varMap t
typeExprToTypeWithEnv env aliasMap paramAliasMap varMap (TyExprParens t) =
  typeExprToTypeWithEnv env aliasMap paramAliasMap varMap t
typeExprToTypeWithEnv env aliasMap paramAliasMap varMap (TyExprTuple ts) =
  tTuple (map (typeExprToTypeWithEnv env aliasMap paramAliasMap varMap) ts)

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
        Just info | Array.length info.params == Array.length args ->
          let argTypes = map (substituteTypeExpr aliasMap paramAliasMap paramSubst) args
              nestedSubst = Map.fromFoldable (Array.zip info.params argTypes)
          in substituteTypeExpr aliasMap paramAliasMap nestedSubst info.body
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
  tTuple (map (substituteTypeExpr aliasMap paramAliasMap paramSubst) ts)
substituteTypeExpr aliasMap paramAliasMap paramSubst (TyExprParens t) = substituteTypeExpr aliasMap paramAliasMap paramSubst t
substituteTypeExpr aliasMap paramAliasMap paramSubst (TyExprForAll _ t) = substituteTypeExpr aliasMap paramAliasMap paramSubst t
substituteTypeExpr aliasMap paramAliasMap paramSubst (TyExprConstrained _ t) = substituteTypeExpr aliasMap paramAliasMap paramSubst t

-- | Convert a TypeExpr to a Type using the variable mapping
typeExprToType :: Map.Map String TVar -> TypeExpr -> Type
typeExprToType varMap (TyExprVar name) =
  case Map.lookup name varMap of
    Just tv -> TyVar tv
    Nothing -> TyCon (mkTCon name [])  -- Assume it's a type constructor
typeExprToType varMap (TyExprCon name) =
  -- Handle well-known type aliases
  case name of
    -- Type wildcard/hole - use a special placeholder type var
    "_" -> TyVar (mkTVar (-999) "_")
    "TCon" -> TyRecord { fields: Map.fromFoldable [Tuple "name" tString, Tuple "args" (tArray tTypeHolder)], row: Nothing }
    "TVar" -> TyRecord { fields: Map.fromFoldable [Tuple "id" tInt, Tuple "name" tString], row: Nothing }
    "Token" -> TyRecord { fields: Map.fromFoldable [Tuple "tokenType" (TyCon (mkTCon "TokenType" [])), Tuple "value" tString, Tuple "line" tInt, Tuple "column" tInt, Tuple "pos" tInt], row: Nothing }
    -- Type aliases from Types.purs
    "Subst" -> TyCon (mkTCon "Map" [tInt, tTypeHolder])
    "Env" -> TyRecord { fields: Map.fromFoldable [Tuple "bindings" (TyCon (mkTCon "Map" [tString, tSchemeHolder])), Tuple "counter" tInt, Tuple "registryLayer" (TyCon (mkTCon "Maybe" [tInt])), Tuple "namespace" (TyCon (mkTCon "Maybe" [tString]))], row: Nothing }
    "Scheme" -> TyRecord { fields: Map.fromFoldable [Tuple "vars" (tArray tTVarHolder), Tuple "ty" tTypeHolder], row: Nothing }
    -- FunctionDeclaration record type alias
    "FunctionDeclaration" -> TyRecord { fields: Map.fromFoldable [Tuple "name" tString, Tuple "parameters" (tArray tPatternHolder), Tuple "body" tExprHolder, Tuple "guards" (tArray tGuardedExprHolder), Tuple "typeSignature" (TyCon (mkTCon "Maybe" [tTypeSigHolder])), Tuple "whereBindings" (tArray tLetBindHolder)], row: Nothing }
    "DataConstructor" -> TyRecord { fields: Map.fromFoldable [Tuple "name" tString, Tuple "fields" (tArray tDataFieldHolder)], row: Nothing }
    "DataField" -> TyRecord { fields: Map.fromFoldable [Tuple "name" (TyCon (mkTCon "Maybe" [tString])), Tuple "ty" tTypeExprHolder], row: Nothing }
    "TypeSignature" -> TyRecord { fields: Map.fromFoldable [Tuple "name" tString, Tuple "typeVars" (tArray tString), Tuple "constraints" (tArray tConstraintHolder), Tuple "ty" tTypeExprHolder], row: Nothing }
    "LetBind" -> TyRecord { fields: Map.fromFoldable [Tuple "pattern" tPatternHolder, Tuple "value" tExprHolder, Tuple "typeAnn" (TyCon (mkTCon "Maybe" [tTypeExprHolder]))], row: Nothing }
    "Ast.LetBind" -> TyRecord { fields: Map.fromFoldable [Tuple "pattern" tPatternHolder, Tuple "value" tExprHolder, Tuple "typeAnn" (TyCon (mkTCon "Maybe" [tTypeExprHolder]))], row: Nothing }
    "CaseClause" -> TyRecord { fields: Map.fromFoldable [Tuple "pattern" tPatternHolder, Tuple "guard" (TyCon (mkTCon "Maybe" [tExprHolder])), Tuple "body" tExprHolder], row: Nothing }
    "Ast.CaseClause" -> TyRecord { fields: Map.fromFoldable [Tuple "pattern" tPatternHolder, Tuple "guard" (TyCon (mkTCon "Maybe" [tExprHolder])), Tuple "body" tExprHolder], row: Nothing }
    "GuardedExpr" -> TyRecord { fields: Map.fromFoldable [Tuple "guards" (tArray tGuardClauseHolder), Tuple "body" tExprHolder], row: Nothing }
    "Ast.GuardedExpr" -> TyRecord { fields: Map.fromFoldable [Tuple "guards" (tArray tGuardClauseHolder), Tuple "body" tExprHolder], row: Nothing }
    "GuardClause" -> TyCon (mkTCon "GuardClause" [])  -- GuardClause is an ADT with constructors GuardExpr and GuardPat
    "Ast.GuardClause" -> TyCon (mkTCon "GuardClause" [])
    "Constraint" -> TyRecord { fields: Map.fromFoldable [Tuple "className" tString, Tuple "types" (tArray tTypeExprHolder)], row: Nothing }
    "Ast.Constraint" -> TyRecord { fields: Map.fromFoldable [Tuple "className" tString, Tuple "types" (tArray tTypeExprHolder)], row: Nothing }
    "TypeAlias" -> TyRecord { fields: Map.fromFoldable [Tuple "name" tString, Tuple "typeVars" (tArray tString), Tuple "ty" tTypeExprHolder], row: Nothing }
    "Ast.TypeAlias" -> TyRecord { fields: Map.fromFoldable [Tuple "name" tString, Tuple "typeVars" (tArray tString), Tuple "ty" tTypeExprHolder], row: Nothing }
    "DataType" -> TyRecord { fields: Map.fromFoldable [Tuple "name" tString, Tuple "typeVars" (tArray tString), Tuple "constructors" (tArray tDataConstructorHolder)], row: Nothing }
    "Ast.DataType" -> TyRecord { fields: Map.fromFoldable [Tuple "name" tString, Tuple "typeVars" (tArray tString), Tuple "constructors" (tArray tDataConstructorHolder)], row: Nothing }
    "DataConstructor" -> TyRecord { fields: Map.fromFoldable [Tuple "name" tString, Tuple "fields" (tArray tDataFieldHolder), Tuple "isRecord" tBool], row: Nothing }
    "Ast.DataConstructor" -> TyRecord { fields: Map.fromFoldable [Tuple "name" tString, Tuple "fields" (tArray tDataFieldHolder), Tuple "isRecord" tBool], row: Nothing }
    "ImportDeclaration" -> TyRecord { fields: Map.fromFoldable [Tuple "moduleName" tString, Tuple "alias" (TyCon (mkTCon "Maybe" [tString])), Tuple "items" (tArray (TyCon (mkTCon "ImportItem" []))), Tuple "hiding" tBool], row: Nothing }
    "ModuleDeclaration" -> TyRecord { fields: Map.fromFoldable [Tuple "name" tString], row: Nothing }
    "GenCtx" -> TyRecord { fields: Map.fromFoldable [Tuple "moduleFuncs" (TyCon (mkTCon "Set" [tString])), Tuple "locals" (TyCon (mkTCon "Set" [tString]))], row: Nothing }
    "Module" -> TyRecord { fields: Map.fromFoldable [Tuple "name" tString, Tuple "declarations" (tArray (TyCon (mkTCon "Declaration" [])))], row: Nothing }
    _ -> TyCon (mkTCon name [])
  where
    -- Avoid circular dependency with tType
    tTypeHolder = TyCon (mkTCon "Type" [])
    tTVarHolder = TyRecord { fields: Map.fromFoldable [Tuple "id" tInt, Tuple "name" tString], row: Nothing }
    tSchemeHolder = TyRecord { fields: Map.fromFoldable [Tuple "vars" (tArray tTVarHolder), Tuple "ty" tTypeHolder], row: Nothing }
    tPatternHolder = TyCon (mkTCon "Pattern" [])
    tExprHolder = TyCon (mkTCon "Expr" [])
    tTypeSigHolder = TyRecord { fields: Map.fromFoldable [Tuple "name" tString, Tuple "typeVars" (tArray tString), Tuple "constraints" (tArray tConstraintHolder), Tuple "ty" tTypeExprHolder], row: Nothing }
    tLetBindHolder = TyRecord { fields: Map.fromFoldable [Tuple "pattern" tPatternHolder, Tuple "value" tExprHolder, Tuple "typeAnn" (TyCon (mkTCon "Maybe" [tTypeExprHolder]))], row: Nothing }
    tGuardedExprHolder = TyRecord { fields: Map.fromFoldable [Tuple "guards" (tArray tGuardClauseHolder), Tuple "body" tExprHolder], row: Nothing }
    tGuardClauseHolder = TyCon (mkTCon "GuardClause" [])  -- ADT not a record
    tDataFieldHolder = TyRecord { fields: Map.fromFoldable [Tuple "label" tString, Tuple "ty" tTypeExprHolder], row: Nothing }
    tDataConstructorHolder = TyRecord { fields: Map.fromFoldable [Tuple "name" tString, Tuple "fields" (tArray tDataFieldHolder), Tuple "isRecord" tBool], row: Nothing }
    tTypeExprHolder = TyCon (mkTCon "TypeExpr" [])
    tConstraintHolder = TyRecord { fields: Map.fromFoldable [Tuple "className" tString, Tuple "types" (tArray tTypeExprHolder)], row: Nothing }
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
  let varList = Array.mapWithIndex (\i name -> Tuple name (mkTVar (-(i + 1)) name)) vars
      newVarMap = Map.union (Map.fromFoldable varList) varMap  -- New vars shadow old ones
  in typeExprToType newVarMap t
typeExprToType varMap (TyExprConstrained _ t) = typeExprToType varMap t  -- Ignore constraints
typeExprToType varMap (TyExprParens t) = typeExprToType varMap t
typeExprToType varMap (TyExprTuple ts) =
  -- Use tTuple to normalize tuple names (Tuple for 2-tuples)
  tTuple (map (typeExprToType varMap) ts)

-- | Process a type alias declaration
checkTypeAlias :: Env -> TypeAlias -> Env
checkTypeAlias env ta =
  -- Convert the type expression to an actual Type and store it
  let ty = typeExprToType Map.empty ta.ty
      scheme = mkScheme [] ty
  in extendEnv env ta.name scheme

-- | Type check a module with two-pass approach for forward references
-- Pass 1: Process data types, type aliases, and collect function signatures
-- Pass 2: Type check function bodies
checkModule :: Env -> Array Declaration -> Either TCError Env
checkModule env decls =
  -- Pass 1: Process non-function declarations and collect function names
  let env1 = processNonFunctions env decls
      -- Pass 2: Add placeholder types for all functions first
      env2 = addFunctionPlaceholders env1 decls
  -- Pass 3: Type check all function bodies
  in checkFunctionBodies env2 decls

-- | Collect type aliases from declarations into a Map String Type
-- | For non-parameterized aliases only (backward compatible)
collectTypeAliases :: Array Declaration -> Map.Map String Type
collectTypeAliases decls = Array.foldl collect Map.empty decls
  where
    collect m (DeclTypeAlias ta)
      | Array.null ta.typeVars = Map.insert ta.name (typeExprToType Map.empty ta.ty) m
      | otherwise = m  -- Skip parameterized aliases here
    collect m _ = m

-- | Collect parameterized type aliases into a Map String TypeAliasInfo
collectParamTypeAliases :: Array Declaration -> Map.Map String TypeAliasInfo
collectParamTypeAliases decls = Array.foldl collect Map.empty decls
  where
    collect m (DeclTypeAlias ta) = Map.insert ta.name { params: ta.typeVars, body: ta.ty } m
    collect m _ = m

-- | Process non-function declarations (data types, type aliases, imports, type sigs)
-- Process in two phases: type aliases first, then data types
-- This ensures type aliases are available when processing data constructor fields
processNonFunctions :: Env -> Array Declaration -> Env
processNonFunctions env decls =
  let -- Phase 1: Collect all type aliases into maps
      aliasMap = collectTypeAliases decls
      paramAliasMap = collectParamTypeAliases decls
      -- Also add aliases to env for other purposes
      env1 = Array.foldl processTypeAlias env decls
      -- Phase 2: Process data types (with type aliases available)
      env2 = Array.foldl (processDataType aliasMap paramAliasMap) env1 decls
  in env2
  where
    processTypeAlias e (DeclTypeAlias ta) = checkTypeAlias e ta
    processTypeAlias e _ = e

    processDataType aliasM paramAliasM e (DeclDataType dt) = checkDataTypeWithAliases aliasM e dt
    processDataType _ _ e _ = e

-- | Add placeholder types for all functions
-- Uses type signature if available, otherwise creates a fresh type variable
addFunctionPlaceholders :: Env -> Array Declaration -> Env
addFunctionPlaceholders env decls =
  let -- Collect type aliases for expanding type signatures
      aliasMap = collectTypeAliases decls
      paramAliasMap = collectParamTypeAliases decls
      -- Collect standalone type signatures into a map
      sigMap = Array.foldl collectSig Map.empty decls
      -- Add placeholders for all functions
  in Array.foldl (addPlaceholder aliasMap paramAliasMap sigMap) env decls
  where
    collectSig m (DeclTypeSig sig) = Map.insert sig.name sig.ty m
    collectSig m _ = m

    addPlaceholder aliasM paramAliasM sigs e (DeclFunction func) =
      -- First check embedded type signature in the function (func.typeSignature is Maybe TypeSignature)
      case func.typeSignature of
        Just sig ->
          -- sig is a TypeSignature record with { name, typeVars, constraints, ty }
          let ty = typeExprToTypeWithAllAliases aliasM paramAliasM Map.empty sig.ty
              scheme = mkScheme [] ty
          in extendEnv e func.name scheme
        Nothing ->
          -- Then check standalone signatures
          case Map.lookup func.name sigs of
            Just tyExpr ->
              let ty = typeExprToTypeWithAllAliases aliasM paramAliasM Map.empty tyExpr
                  scheme = mkScheme [] ty
              in extendEnv e func.name scheme
            Nothing ->
              -- No signature, add fresh type variable
              let Tuple tv e' = freshVar e ("fn_" <> func.name)
              in extendEnv e' func.name (mkScheme [] (TyVar tv))
    addPlaceholder _ _ _ e _ = e

-- | Type check all function bodies
-- Handles multi-clause functions by merging adjacent declarations with the same name
checkFunctionBodies :: Env -> Array Declaration -> Either TCError Env
checkFunctionBodies env decls =
  let mergedDecls = mergeMultiClauseFunctions decls
  in go env mergedDecls
  where
    go e [] = Right e
    go e ds = case Array.uncons ds of
      Nothing -> Right e
      Just { head: DeclFunction func, tail: rest } ->
        case checkFunction e func of
          Left err -> Left err
          Right r -> go r.env rest
      Just { head: _, tail: rest } -> go e rest

-- | Merge adjacent function declarations with the same name into single functions
-- Converts: f (Pat1) = body1; f (Pat2) = body2
-- Into:     f x = case x of Pat1 -> body1; Pat2 -> body2
mergeMultiClauseFunctions :: Array Declaration -> Array Declaration
mergeMultiClauseFunctions decls = Array.fromFoldable (go (Array.toUnfoldable decls) [])
  where
    go :: List Declaration -> Array Declaration -> List Declaration
    go Nil acc = List.fromFoldable (Array.reverse acc)
    go (Cons d rest) acc = case d of
      DeclFunction func ->
        -- Collect all adjacent functions with the same name
        let Tuple sameName remaining = collectSameName func.name rest Nil
            allClauses = Array.cons func (Array.fromFoldable sameName)
        in if Array.length allClauses > 1
           then go remaining (Array.cons (DeclFunction (mergeClausesIntoOne allClauses)) acc)
           else go rest (Array.cons d acc)
      _ -> go rest (Array.cons d acc)

    -- Collect adjacent DeclFunction with same name
    collectSameName :: String -> List Declaration -> List FunctionDeclaration -> Tuple (List FunctionDeclaration) (List Declaration)
    collectSameName name Nil acc = Tuple acc Nil
    collectSameName name (Cons (DeclFunction f) rest) acc
      | f.name == name = collectSameName name rest (List.Cons f acc)
    collectSameName _ remaining acc = Tuple acc remaining

    -- Merge multiple function clauses into one with case expression
    mergeClausesIntoOne :: Array FunctionDeclaration -> FunctionDeclaration
    mergeClausesIntoOne clauses =
      case Array.head clauses of
        Nothing -> { name: "", parameters: [], body: ExprVar "error", guards: [], typeSignature: Nothing }
        Just first ->
          let name = first.name
              -- All clauses should have same number of parameters
              numParams = Array.length first.parameters
              -- Create fresh parameter names
              paramNames = Array.mapWithIndex (\i _ -> "__arg" <> show i) first.parameters
              paramPats = map PatVar paramNames
              paramVars = map ExprVar paramNames
              -- Build case clauses from each function clause
              caseClauses = Array.mapMaybe (clauseToCaseClause paramVars) clauses
              -- Build case expression or tuple case for multiple params
              caseExpr = case numParams of
                0 -> first.body  -- No params, just use first body
                1 -> case Array.head paramVars of
                  Just v -> ExprCase v caseClauses
                  Nothing -> first.body
                _ -> ExprCase (ExprTuple paramVars) (Array.mapMaybe (clauseToTupleCase paramVars) clauses)
          in { name
             , parameters: paramPats
             , body: caseExpr
             , guards: []
             , typeSignature: first.typeSignature
             }

    -- Convert a function clause to a case clause (single parameter case)
    clauseToCaseClause :: Array Expr -> FunctionDeclaration -> Maybe CaseClause
    clauseToCaseClause _ func = case Array.head func.parameters of
      Nothing -> Nothing
      Just pat -> Just { pattern: pat, body: func.body, guard: Nothing }

    -- Convert a function clause to a case clause with tuple pattern (multi-param)
    clauseToTupleCase :: Array Expr -> FunctionDeclaration -> Maybe CaseClause
    clauseToTupleCase _ func =
      let n = Array.length func.parameters
          -- Use "Tuple" for 2 params, "Tuple3" for 3, etc. to match PureScript conventions
          tupName = if n == 2 then "Tuple" else "Tuple" <> show n
      in if n > 1
         then Just { pattern: PatCon tupName func.parameters, body: func.body, guard: Nothing }
         else clauseToCaseClause [] func
