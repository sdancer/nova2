# Hardcoded Items Report

This report documents all hardcoded values, lists, and mappings in the Nova compiler's CodeGen.purs and TypeChecker.purs modules. These represent areas where the compiler has special-case handling that may need to be made more generic or configurable.

## CodeGen.purs

### 1. Module Name Translations (lines 244-256, 968-1013)

The `translateImportModule` and `translateQualified` functions contain hardcoded mappings from PureScript module names to Elixir/Nova modules:

**Standard Library Mappings:**
```purescript
"Prelude" -> "Nova.Runtime"
"Data.Foldable" -> "Nova.Runtime"
"Data.List" -> "Nova.Runtime"
"Data.Array" -> "Nova.Runtime"
"Data.String" -> "Nova.Runtime"
"Data.String.CodeUnits" -> "Nova.Runtime"
"Data.Maybe" -> "Nova.Runtime"
"Data.Either" -> "Nova.Runtime"
"Data.Tuple" -> "Nova.Runtime"
"Data.Map" -> "Nova.Runtime"
"Data.Set" -> "Nova.Runtime"
```

**Qualified Module Mappings:**
```purescript
"Map" -> "Nova.Map"
"Data.Map" -> "Nova.Map"
"Set" -> "Nova.Set"
"Data.Set" -> "Nova.Set"
"Array" -> "Nova.Array"
"Data.Array" -> "Nova.Array"
"List" -> "Nova.List"
"Data.List" -> "Nova.List"
"String" -> "Nova.String"
"Data.String" -> "Nova.String"
"SCU" / "CU" -> "Nova.String"
```

**Special Function Translations:**
```purescript
"Int.fromString" -> "Nova.String.to_int"
"Data.Int.fromString" -> "Nova.String.to_int"
"Number.fromString" -> "Nova.String.to_float"
"Data.Number.fromString" -> "Nova.String.to_float"
```

### 2. Function Arity Tables (lines 260-332, 928-956)

**Imported Function Arities (`getImportedFuncArity`):**
```purescript
"pure" -> 1
"bind" -> 2
"const" -> 2
"identity" -> 1
"flip" -> 3
"compose" -> 2
"show" -> 1
"length" -> 1
"head" / "tail" / "null" / "reverse" / "concat" -> 1
"map" / "filter" -> 2
"foldl" / "foldr" -> 3
"elem" / "find" / "take" / "drop" -> 2
"lookup" -> 2
"insert" -> 3
"delete" / "member" -> 2
"singleton" -> 1
"empty" -> 0
"cons" / "snoc" / "append" -> 2
"zipWith" -> 3
"zip" -> 2
"fst" / "snd" -> 1
-- ... many more
```

**Types Module Function Arities:**
- Arity 0: `emptySubst`, `emptyEnv`, `emptyExports`, `tInt`, `tString`, `tBool`, `tChar`, `tNumber`
- Arity 1: `singleSubst`, `mkTVar`, `mkTCon0`, `tArray`, `tMaybe`, `tSet`, `tList`, `tTuple`
- Arity 2: `composeSubst`, `applySubst`, `mkTCon`, `tArrow`, `tEither`, `tMap`, `extendEnv`, `lookupEnv`, `freshVar`, `generalize`, `instantiate`

**Unify Module Function Arities:**
```purescript
"unify" -> 2
"unifyMany" -> 2
"bindVar" -> 2
"occurs" -> 2
"unifyRecords" -> 2
"unifyField" -> 4
```

### 3. Data Constructor Lists (lines 726-764, 785-792, 865-894)

**`isDataConstructor` - All Known Data Constructors (~80 items):**
- Tuple: `Tuple`, `Tuple2`, `Tuple3`, `Tuple4`, `Tuple5`
- Maybe/Either: `Just`, `Nothing`, `Left`, `Right`
- List: `Cons`, `Nil`
- Type constructors: `TyVar`, `TyCon`, `TyRecord`
- Token types: `TokKeyword`, `TokIdentifier`, etc.
- Expression constructors: `ExprVar`, `ExprLit`, `ExprApp`, etc.
- Pattern constructors: `PatVar`, `PatWildcard`, etc.
- Declaration constructors: `DeclFunction`, `DeclDataType`, etc.
- Type expression constructors: `TyExprCon`, `TyExprVar`, etc.
- Guard clause constructors: `GuardExpr`, `GuardPat`
- Newtype constructors: `Parser`

**`arity2Constructors` - 2-Argument Constructors:**
```purescript
"ExprApp", "ExprCase", "ExprBinOp", "ExprRecordAccess", "ExprRecordUpdate"
"ExprQualified", "ExprTyped", "ExprUnaryOp", "ExprLambda", "ExprLet"
"PatCon", "PatCons", "PatAs"
"TyExprApp", "TyExprArrow", "TyExprRecord", "TyExprForAll", "TyExprConstrained"
"DoBind", "GuardPat", "ImportType"
```

**`isNullaryConstructor` - Zero-Argument Constructors (~45 items):**
- Token types: `TokKeyword`, `TokIdentifier`, etc.
- Pattern: `PatWildcard`
- Import specs: `ImportAll`, `ImportNone`
- Declaration kinds: `KindFunction`, `KindDataType`, etc.
- Associativity: `AssocLeft`, `AssocRight`, `AssocNone`
- Layout delimiters: `LytRoot`, `LytTopDecl`, `LytCase`, etc.
- CST Token types: `TokLeftParen`, `TokRightParen`, etc.
- Fixity: `Infix`, `Infixl`, `Infixr`
- Source style: `ASCII`, `Unicode`

### 4. AST Constructor Lists (lines 797-823)

**`isAstConstructor` - Constructors from Nova.Compiler.Ast:**
- Expression: `ExprVar`, `ExprLit`, `ExprApp`, `ExprLambda`, `ExprLet`, `ExprIf`, `ExprCase`, `ExprBinOp`, `ExprList`, `ExprRecord`, `ExprRecordAccess`, `ExprParens`, `ExprDo`, `ExprQualified`, `ExprRecordUpdate`, `ExprTyped`, `ExprUnaryOp`, `ExprTuple`, `ExprSection`, `ExprSectionLeft`, `ExprSectionRight`, `ExprNegate`
- Pattern: `PatVar`, `PatWildcard`, `PatLit`, `PatCon`, `PatRecord`, `PatList`, `PatCons`, `PatAs`, `PatParens`, `PatTyped`
- Literal: `LitInt`, `LitString`, `LitChar`, `LitBool`, `LitNumber`
- Declaration: `DeclFunction`, `DeclTypeSig`, `DeclDataType`, `DeclTypeAlias`, `DeclModule`, `DeclImport`, `DeclTypeClass`, `DeclTypeClassInstance`, `DeclInfix`, `DeclForeignImport`, `DeclType`
- Type expression: `TyExprCon`, `TyExprVar`, `TyExprApp`, `TyExprArrow`, `TyExprRecord`, `TyExprForAll`, `TyExprTuple`, `TyExprConstrained`, `TyExprParens`
- Do statement: `DoLet`, `DoBind`, `DoExpr`
- Guard clause: `GuardExpr`, `GuardPat`
- Token types: `TokKeyword`, `TokIdentifier`, etc.

### 5. Module Function Lists (lines 900-918, 941-956)

**`isTypesModuleFunc` - Functions from Nova.Compiler.Types:**
```purescript
"emptySubst", "singleSubst", "composeSubst", "applySubst"
"freeTypeVars", "freeTypeVarsScheme", "freeTypeVarsEnv"
"lookupSubst", "extendEnv", "lookupEnv", "applySubstToEnv"
"freshVar", "generalize", "instantiate", "mkScheme"
"mkTVar", "mkTCon", "mkTCon0", "tyVar", "tyCon", "tyRecord"
"tInt", "tString", "tBool", "tChar", "tArray", "tArrow"
"tMaybe", "tEither", "tTuple", "tMap", "tSet", "tList", "tNumber"
"emptyEnv", "builtinPrelude"
"emptyExports", "mergeTypeExport", "mergeExportsToEnv", "registerModule", "lookupModule"
```

**`isUnifyModuleFunc` - Functions from Nova.Compiler.Unify:**
```purescript
"unify", "unifyMany", "bindVar", "occurs", "unifyRecords", "unifyField"
```

### 6. Prelude Function Names (lines 839-855)

**`preludeFuncNames` - Auto-injected Prelude Functions (~55 items):**
```purescript
"pure", "bind", "const", "identity", "flip", "compose"
"show", "length", "head", "tail", "null", "reverse", "concat"
"sum", "product", "maximum", "minimum", "fromMaybe", "maybe"
"map", "filter", "foldl", "foldr", "foldMap", "elem", "find"
"findIndex", "take", "drop", "lookup", "insert", "delete", "member"
"singleton", "empty", "cons", "snoc", "append", "intercalate"
"replicate", "concatMap", "any", "all", "zipWith", "zip", "unzip"
"sortBy", "sort", "nub", "union", "intersect", "difference"
"fst", "snd", "split", "joinWith", "trim", "toLower", "toUpper"
"contains", "indexOf", "replaceAll", "charAt", "toCharArray", "fromCharArray"
```

### 7. Binary Function Detection (lines 896-902, 454-460)

**`isBinaryFunc` - Known 2-Argument Functions:**
```purescript
"applySubst", "lookupSubst", "singleSubst", "composeSubst"
"extendEnv", "lookupEnv", "unify", "bindVar"
```

**`isPartialAppOfArity2` - Partial Application Detection:**
```purescript
Array.filter, Array.map, Array.find, Array.any, Array.all
Array.takeWhile, Array.dropWhile, Array.sortBy, Array.groupBy
filter, map, find, any, all (unqualified)
```

### 8. Special Variable Names (lines 1076-1088)

**Hardcoded Variable Translations:**
```purescript
"Nothing" -> ":nothing"
"nothing" -> ":nothing"
"Nil" -> "[]"
"otherwise" -> "true"
"True" -> "true"
"False" -> "false"
"not" -> "(&Kernel.not/1)"
"mod" -> "(&rem/2)"
"unit" -> ":unit"
"intToString" -> "(&to_string/1)"
"__guarded__" -> ":__guarded__"
```

### 9. Special Operators (lines 1343-1418)

**Hardcoded Operator Translations:**
```purescript
":" -> List cons "[h | t]"
"<>" -> "Nova.Runtime.append(l, r)"
"<<<" -> Function composition (right-to-left)
">>>" -> Function composition (left-to-right)
"$" -> Function application
"#" -> Reverse application
"<$>" -> "Nova.Runtime.fmap(f, x)"
"<|>" -> "Nova.Runtime.alt(x, y)"
"*>" -> "Nova.Runtime.seq(x, y)"
"<*" -> "Nova.Runtime.seq_left(x, y)"
```

---

## TypeChecker.purs

### 1. Type Alias Definitions (lines 965-1023)

**`typeExprToType` - Hardcoded Type Aliases:**

These are record type structures for internal compiler types:

```purescript
"_" -> TyVar (type wildcard/hole)
"Boolean" -> tBool

"TCon" -> {name: String, args: Array Type}
"TVar" -> {id: Int, name: String}
"Token" -> {tokenType: TokenType, value: String, line: Int, column: Int, pos: Int}
"Subst" -> Map Int Type
"Env" -> {bindings: Map String Scheme, counter: Int, registryLayer: Maybe Int, namespace: Maybe String}
"Scheme" -> {vars: Array TVar, ty: Type}

"FunctionDeclaration" -> {name: String, parameters: Array Pattern, body: Expr, guards: Array GuardedExpr, typeSignature: Maybe TypeSig, whereBindings: Array LetBind}
"DataConstructor" -> {name: String, fields: Array DataField}
"DataField" -> {name: Maybe String, ty: TypeExpr}
"TypeSignature" -> {name: String, typeVars: Array String, constraints: Array Constraint, ty: TypeExpr}
"LetBind" -> {pattern: Pattern, value: Expr, typeAnn: Maybe TypeExpr}
"CaseClause" -> {pattern: Pattern, guard: Maybe Expr, body: Expr}
"GuardedExpr" -> {guards: Array GuardClause, body: Expr}
"Constraint" -> {className: String, types: Array TypeExpr}
"TypeAlias" -> {name: String, typeVars: Array String, ty: TypeExpr}
"DataType" -> {name: String, typeVars: Array String, constructors: Array DataConstructor}
"ImportDeclaration" -> {moduleName: String, alias: Maybe String, items: Array ImportItem, hiding: Bool}
"ModuleDeclaration" -> {name: String}
"GenCtx" -> {moduleFuncs: Set String, locals: Set String}
"Module" -> {name: String, declarations: Array Declaration}
"TypeAliasInfo" -> {params: Array String, body: TypeExpr}
"TypeInfo" -> {arity: Int, constructors: Array String}
```

**Also with "Ast." prefix variants for qualified names.**

### 2. Record Type Alias Names (Unify.purs, lines 93-151)

**`isRecordTypeAlias` - Types that unify with records:**
```purescript
"TypeClass", "TypeClassInstance", "NewtypeDecl", "FunctionDecl"
"FunctionDeclaration", "TypeSig", "DataType", "DataConstructor"
"DataField", "TypeAlias", "ModuleDecl", "ModuleExports"
"ImportDecl", "InfixDecl", "Constraint", "ForeignImport"
"TypeDecl", "LetBind", "CaseClause", "GuardedExpr", "PatResult"
"InstantiateResult", "InferResult", "Env", "Scheme", "TVar", "LiftedLambda"
```

---

## Summary

### Areas for Future Improvement

1. **Module Mappings**: Could be made configurable via a configuration file or module registry
2. **Function Arities**: Could be inferred from type signatures during compilation
3. **Constructor Lists**: Could be dynamically collected from parsed declarations
4. **Type Aliases**: Already partially dynamic via `collectTypeAliases`, but hardcoded fallbacks remain
5. **Operator Translations**: Could be defined in a data structure rather than pattern matches

### Statistics

| Category | CodeGen.purs | TypeChecker.purs | Total |
|----------|--------------|------------------|-------|
| Module mappings | ~25 | 0 | ~25 |
| Function arities | ~100 | 0 | ~100 |
| Constructor names | ~130 | ~30 | ~160 |
| Type aliases | 0 | ~30 | ~30 |
| Special variables | ~12 | 0 | ~12 |
| Operator translations | ~10 | 0 | ~10 |
| **Total** | **~277** | **~60** | **~337** |

