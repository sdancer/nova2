# PureScript Passing Tests Analysis

Analysis of 365 test files from the official PureScript compiler test suite (`tests/purs/passing`).

## Test Feature Groups Summary

| Feature | Tests | Description |
|---------|-------|-------------|
| **effect_console** | 360 | Require `Effect.Console` (log function) |
| **prelude** | 260 | Require `Prelude` standard library |
| **type_classes** | 139 | Use type classes/instances |
| **do_notation** | 125 | Use do-notation |
| **rank_n_types** | 62 | Multiple `forall` quantifiers |
| **self_contained** | 56 | Minimal dependencies, could run now |
| **kind_signatures** | 54 | Higher-kinded types |
| **fundeps** | 36 | Functional dependencies |
| **test_assert** | 30 | Require `Test.Assert` library |
| **multi_module** | 29 | Import local modules (A, B, M1, etc) |
| **type_level** | 26 | Type-level programming (Proxy, Prim.Row) |
| **ffi** | 24 | Foreign function interface |
| **data_structures** | 17 | Maybe, Tuple, Foldable |
| **unsafe** | 14 | Partial/Unsafe operations |
| **row_polymorphism** | 5 | Row types with `| r` |
| **visible_type_app** | 4 | Visible type applications |
| **st_monad** | 3 | ST mutable state |
| **generics** | 3 | Generic deriving |
| **profunctor** | 3 | Profunctor/Bifunctor |
| **ado_notation** | 2 | Applicative do notation |

## Import Frequency

```
282 import Effect.Console (log)
245 import Prelude
 51 import Effect.Console
 37 import Effect
 23 import Effect (Effect)
 20 import Effect.Console (log, logShow)
 18 import Test.Assert
 14 import Type.Proxy (Proxy(..))
  7 import Partial.Unsafe (unsafePartial)
  7 import A
  6 import Data.Maybe (Maybe(..))
  6 import Data.Eq (class Eq1)
  5 import Test.Assert (assert')
  5 import M1
  4 import Lib
  4 import Effect.Console (logShow, log)
  4 import Data.Tuple (Tuple)
  4 import Data.Predicate (Predicate)
```

## Priority Batches for Nova Implementation

### Batch 1 - Self-Contained (56 tests)
Tests with minimal external dependencies that just need `log` implemented:
- `1185.purs`
- `1570.purs`
- `1881.purs`
- `2252.purs`
- `2378.purs`
- `2438.purs`
- `2626.purs`
- `2941.purs`
- `2958.purs`
- `3238.purs`
- `3481.purs`
- `3830.purs`
- `4038.purs`
- `4180.purs`
- `4194.purs`
- `Applicative.purs`
- `Church.purs`
- `Conditional.purs`
- `ConstraintOutsideForall.purs`
- `ConstraintParsingIssue.purs`
- `Dollar.purs`
- `FunWithFunDeps.purs`
- `FunctionalDependencies.purs`
- `HasOwnProperty.purs`
- `InstanceSigs.purs`
- `InstanceSigsGeneral.purs`
- `KindUnificationInSolver.purs`
- `LargeSumType.purs`
- `Monad.purs`
- `NakedConstraint.purs`
- `ParseTypeInt.purs`
- `PolykindBindingGroup1.purs`
- `PolykindBindingGroup2.purs`
- `PolykindGeneralization.purs`
- `PolykindGeneralizationHygiene.purs`
- `PolykindGeneralizedTypeSynonym.purs`
- `PolykindInstantiatedInstance.purs`
- `PolykindInstantiation.purs`
- `QuantifiedKind.purs`
- `Rank2Kinds.purs`
- `ReExportsExported.purs`
- `RowsInKinds.purs`
- `RowsInKinds2.purs`
- `StandaloneKindSignatures.purs`
- `TypeSynonymInstance2.purs`
- `TypeSynonymInstance3.purs`
- `TypeSynonymInstance4.purs`
- `TypeSynonymInstance5.purs`
- `TypeSynonymsInKinds.purs`
- `UTF8Sourcefile.purs`
- `UnicodeIdentifier.purs`
- `UnicodeOperators.purs`
- `UnifyInTypeInstanceLookup.purs`
- `UsableTypeClassMethods.purs`
- `iota.purs`

### Batch 2 - Effect.Console Only (~100 tests)
Need `log` function but minimal other stdlib. Just implement:
```elixir
def log(msg), do: IO.puts(msg)
```

### Batch 3 - Prelude (~150 tests)
Need basic Prelude functions:
- `Eq` class with `==`, `/=`
- `Ord` class with `compare`, `<`, `>`, `<=`, `>=`
- `Show` class with `show`
- `Functor` class with `map`
- `Apply`, `Applicative`, `Bind`, `Monad`
- `Semigroup`, `Monoid`
- Basic operators: `$`, `#`, `<<<`, `>>>`

### Batch 4 - Type Classes/Instances (139 tests)
Require proper instance resolution and method dispatch.

### Batch 5 - FFI (24 tests)
Foreign function interface support - requires JS/runtime interop.

### Batch 6 - Multi-Module (29 tests)
Tests requiring multiple companion modules (A.purs, B.purs, M1.purs, etc.)

## Requirements to Run Most Tests

1. **`log` function** - Most tests just call `log "Done"` as their main
2. **Basic type classes** - Eq, Ord, Show, Functor, Monad
3. **Instance resolution** - Proper type class instance dispatch
4. **Do-notation** - Desugaring to `>>=` and `>>`
5. **Pattern matching** - Case expressions, guards

## Full Test Results (December 2024)

### Latest Results (After Constructor Arity Fix)

| Stage | Count | Percentage |
|-------|-------|------------|
| **Total Tests** | 365 | 100% |
| **Passing** | 194 | 53.2% |
| **Parse Errors** | 62 | 17.0% |
| **Compile Errors** | 0 | 0% |
| **Execute Errors** | 109 | 29.9% |

### Previous Results (Before Fixes)

| Stage | Count | Percentage |
|-------|-------|------------|
| **Total Tests** | 365 | 100% |
| **Passing** | 174 | 47.7% |
| **Parse Errors** | 62 | 17.0% |
| **Compile Errors** | 0 | 0% |
| **Execute Errors** | 129 | 35.3% |

### Progress History
- **Initial**: 174 passing (47.7%)
- **After value/function fix**: 183 passing (50.1%)
- **After constructor arity fix**: 194 passing (53.2%)

### Parse Failure Categories

The 62 parse failures fall into these categories:

**Deriving-related (10 tests):**
- DerivingBifunctor, DerivingContravariant, DerivingFoldable, DerivingFunctor
- DerivingFunctorFromBi, DerivingFunctorPrefersSimplerClasses, DerivingProfunctor
- DerivingTraversable, Eq1Deriving, Eq1InEqDeriving, Ord1Deriving, Ord1InOrdDeriving

**Kind/Type-level (12 tests):**
- ForeignDataInKind, ForeignKind, RowsInKinds, RowsInKinds2
- StandaloneKindSignatures, TypeOperators, VisibleTypeApplications
- SolvingAddInt, SolvingAppendSymbol, SolvingCompareInt, SolvingCompareSymbol, SolvingMulInt

**Advanced Instance Features (10 tests):**
- ClassRefSyntax, Coercible, CyclicInstances, DataConsClassConsOverlapOk
- ExportedInstanceDeclarations, InstanceChain, InstanceNamesGenerated
- InstanceSigs, InstanceSigsGeneral, NewtypeClass, NewtypeInstance

**Functional Dependencies (5 tests):**
- FunWithFunDeps, MPTCs, PolykindInstanceDispatch, EmptyDicts, NewConsClass

**Other Features:**
- GenericsRep (generic deriving)
- ImportHiding (hiding syntax)
- IntToString (type-level)
- PolyLabels, PrimedTypeName
- RowLacks, RowNub
- TCO (tail call optimization annotations)
- AppendInReverse, CSEInitialDigitSymbols, MutRec2, MutRec3

### Execute Failure Analysis

The 129 execute failures include tests that:
1. Require type class instance dispatch (methods on dictionary records)
2. Use Prelude functions not injected (show, map, etc.)
3. Have code generation issues with specific patterns

### Passing Tests Summary

174 tests pass completely - they parse, compile to Elixir, and execute successfully.
Key passing tests include:
- Basic function definitions and applications
- Pattern matching and case expressions
- Lambdas and closures
- Data type declarations
- Record syntax (creation, access, update)
- Type synonyms
- Basic type class definitions
- Let/where bindings
- Do-notation (basic)
- Guard expressions

## Next Steps for Improvement

1. **Parser:** Add support for:
   - `derive` keyword for automatic instance deriving
   - Instance chains (else clauses)
   - Standalone kind signatures
   - Visible type applications (`@Type`)
   - Type-level operators

2. **Code Generation:** Fix:
   - **Value vs function distinction**: When `f = \x -> ...` is defined, calling `f x` should generate `f().(x)` not `f(x)`
   - Type class instances → module-based dispatch (not dictionary passing)
   - Polymorphic let bindings with type annotations
   - Data constructor arity mismatches

3. **Runtime:** Implement:
   - Core Prelude type classes (Eq, Ord, Show, Functor, etc.)
   - Common library functions
