# Nova Self-Hosting Compiler Roadmap

## Goal
Bootstrap a fully self-hosting compiler that can:
1. Parse itself (PureScript source → AST)
2. Type-check itself (AST → Typed AST)
3. Generate Elixir code (Typed AST → .ex files)
4. Run the generated Elixir to compile the next iteration

## Current State

### ✅ Complete
- **Tokenizer**: Full lexical analysis with position tracking
- **Parser**: 1500+ lines covering all syntax forms
- **AST**: Complete node definitions for all language constructs
- **Types**: Core type representation (TVar, TCon, TyRecord, Scheme)
- **Unify**: Unification with occurs check
- **TypeChecker**: Basic Algorithm W (literals, vars, app, lambda, let, if, case, records, lists)
- **CodeGen**: Elixir generation (functions, expressions, data types)
- **E2E Tests**: Generated Elixir executes correctly

### 🔶 Partial
- Case expressions with guards (parsed, codegen works, TC partial)
- Do-notation (parsed, codegen works, TC missing)
- Record patterns (parsed, codegen works, TC missing)

### ❌ Missing for Self-Hosting
- Type class resolution
- Constraint solving
- Module system (imports/exports)
- Foreign imports
- Prelude/standard library types

---

## Phase 1: Core Language Completion
**Goal**: Type-check and compile simple, single-module programs

### 1.1 Complete Pattern Type Inference
- [ ] `PatRecord` - record destructuring
- [ ] `PatList` - list patterns `[a, b, c]`
- [ ] `PatCons` - cons patterns `(h : t)`
- [ ] `PatAs` - as-patterns `x@(Just y)`

### 1.2 Complete Expression Type Inference
- [ ] `ExprRecordUpdate` - `rec { field = value }`
- [ ] `ExprDo` - do-notation (desugar to bind/pure)
- [ ] `ExprSection` - operator sections `(+ 1)`
- [ ] `ExprUnaryOp` - unary operators

### 1.3 Add Standard Operators to Prelude
- [ ] Arithmetic: `+`, `-`, `*`, `/`, `mod`
- [ ] Comparison: `==`, `/=`, `<`, `>`, `<=`, `>=`
- [ ] Boolean: `&&`, `||`, `not`
- [ ] List: `:`, `++`
- [ ] Function: `$`, `.`, `<<<`, `>>>`

### 1.4 Tests
- [ ] E2E: Pattern matching (all patterns)
- [ ] E2E: Record operations
- [ ] E2E: List operations with cons

---

## Phase 2: Type Classes (Minimal)
**Goal**: Support `Show`, `Eq`, `Functor`, `Monad` for basic operations

### 2.1 Type Class Infrastructure
- [ ] Parse type class declarations → `DeclTypeClass`
- [ ] Parse instance declarations → `DeclTypeClassInstance`
- [ ] Store class definitions in environment
- [ ] Store instance definitions

### 2.2 Constraint Collection
- [ ] Collect constraints during inference
- [ ] Propagate constraints through applications
- [ ] Handle `=>` in type signatures

### 2.3 Constraint Resolution
- [ ] Instance lookup by type
- [ ] Dictionary passing transformation
- [ ] Generate instance dictionaries in Elixir

### 2.4 Core Instances
- [ ] `Show` for Int, String, Bool, List, Maybe
- [ ] `Eq` for Int, String, Bool, List
- [ ] `Functor` for List, Maybe
- [ ] `Monad` for Maybe, Either (for do-notation)

---

## Phase 3: Module System
**Goal**: Compile multi-file programs with imports

### 3.1 Module Resolution
- [ ] Parse module headers
- [ ] Track module dependencies
- [ ] Topological sort for compilation order

### 3.2 Import/Export Handling
- [ ] Export all by default
- [ ] Selective imports `import Foo (bar, Baz(..))`
- [ ] Qualified imports `import Foo as F`
- [ ] Hiding `import Foo hiding (bar)`

### 3.3 Cross-Module Type Checking
- [ ] Load type signatures from compiled modules
- [ ] Resolve qualified names
- [ ] Handle re-exports

### 3.4 Code Generation Updates
- [ ] Generate proper Elixir module structure
- [ ] Handle qualified function calls
- [ ] Import statements in generated code

---

## Phase 4: Data Types & Pattern Matching
**Goal**: Full ADT support with exhaustive pattern matching

### 4.1 Data Type Processing
- [ ] Register constructors with their types
- [ ] Generate constructor functions in Elixir
- [ ] Support record syntax in constructors

### 4.2 Pattern Matching Compilation
- [ ] Constructor pattern type checking
- [ ] Nested pattern flattening
- [ ] Guard expression evaluation
- [ ] Exhaustiveness checking (warning)

### 4.3 Generated Elixir Patterns
- [ ] Tagged tuples: `{:Just, x}`
- [ ] Record constructors: `%{__tag__: :Person, name: n}`
- [ ] Optimized boolean/int patterns

---

## Phase 5: Self-Hosting Bootstrap
**Goal**: Compiler can compile itself

### 5.1 Compile Compiler Modules
Order of compilation:
1. [ ] `Nova.Compiler.Ast` - pure data definitions
2. [ ] `Nova.Compiler.Types` - type system types
3. [ ] `Nova.Compiler.Tokenizer` - lexical analysis
4. [ ] `Nova.Compiler.Unify` - unification
5. [ ] `Nova.Compiler.Parser` - syntax analysis
6. [ ] `Nova.Compiler.TypeChecker` - type inference
7. [ ] `Nova.Compiler.CodeGen` - Elixir generation

### 5.2 Runtime Support
- [ ] Prelude module with core functions
- [ ] Effect system (or simple IO monad)
- [ ] String operations
- [ ] Array operations
- [ ] File I/O (for reading source files)

### 5.3 Bootstrap Process
```
┌─────────────────────────────────────────────────────────┐
│  Stage 0: PureScript Compiler (via spago/node)          │
│  ├── Compiles Nova sources → .ex files                  │
│  └── Output: output/elixir/*.ex                         │
├─────────────────────────────────────────────────────────┤
│  Stage 1: Elixir Compiler (via elixir/mix)              │
│  ├── Runs compiled .ex files                            │
│  ├── Compiles Nova sources → .ex files (round 2)        │
│  └── Output: _build/stage1/*.ex                         │
├─────────────────────────────────────────────────────────┤
│  Stage 2: Verify                                        │
│  ├── Compare Stage 0 output vs Stage 1 output           │
│  └── If identical: bootstrap complete!                  │
└─────────────────────────────────────────────────────────┘
```

### 5.4 Verification
- [ ] Diff generated code between stages
- [ ] Run test suite with self-compiled compiler
- [ ] Performance benchmarks

---

## Phase 6: Polish & Optimization
**Goal**: Production-ready compiler

### 6.1 Error Messages
- [ ] Source location in all errors
- [ ] Type mismatch suggestions
- [ ] Undefined variable hints
- [ ] Import resolution errors

### 6.2 Optimization
- [ ] Dead code elimination
- [ ] Inline small functions
- [ ] Tail call optimization markers
- [ ] Constant folding

### 6.3 Developer Experience
- [ ] Watch mode (recompile on change)
- [ ] REPL via Elixir IEx
- [ ] LSP server basics (hover types)

---

## Implementation Priority

### Sprint 1 (Foundation)
1. Pattern inference: `PatRecord`, `PatList`, `PatCons`
2. Operators in prelude with types
3. E2E tests for patterns

### Sprint 2 (Expressions)
1. `ExprRecordUpdate` inference
2. `ExprDo` desugaring + inference
3. E2E tests for do-notation

### Sprint 3 (Type Classes Minimal)
1. Class/instance storage
2. Constraint collection
3. `Show` and `Eq` instances

### Sprint 4 (Modules)
1. Multi-file compilation
2. Import resolution
3. Qualified names

### Sprint 5 (Self-Host Attempt)
1. Compile `Ast.purs`
2. Compile `Tokenizer.purs`
3. Compile `Parser.purs`
4. Iterate on missing features

### Sprint 6+ (Complete Bootstrap)
1. Full compiler compilation
2. Stage comparison
3. Test suite verification

---

## File Organization

```
purs/
├── src/Nova/Compiler/
│   ├── Ast.purs           ✅ Complete
│   ├── Tokenizer.purs     ✅ Complete
│   ├── Parser.purs        ✅ Complete
│   ├── Types.purs         ✅ Complete
│   ├── Unify.purs         ✅ Complete
│   ├── TypeChecker.purs   🔶 Needs: patterns, do, classes
│   ├── CodeGen.purs       ✅ Complete (may need updates)
│   ├── Module.purs        ❌ TODO: module system
│   └── Prelude.purs       ❌ TODO: standard library types
├── test/
│   ├── tokenizer/         ✅
│   ├── parser/            ✅
│   ├── typecheck/         🔶 Needs more coverage
│   └── codegen_elixir/    ✅ E2E working
└── output/elixir/         ❌ TODO: generated .ex files
```

---

## Success Metrics

1. **Phase 1 Complete**: Can compile `factorial` with pattern matching
2. **Phase 2 Complete**: Can use `show` and `==`
3. **Phase 3 Complete**: Can compile multi-file project
4. **Phase 4 Complete**: Can compile ADTs with full pattern matching
5. **Phase 5 Complete**: Compiler compiles itself, output matches
6. **Phase 6 Complete**: Error messages are helpful, compilation is fast

---

## Dependencies & Risks

### Dependencies
- Elixir 1.14+ for running generated code
- Node.js for running PureScript compiler
- spago for PureScript package management

### Risks
- **Type class complexity**: May need to simplify to dictionary-passing
- **Module cycles**: Need careful ordering
- **Performance**: Generated Elixir may be slow initially
- **FFI**: May need escape hatch for some operations

### Mitigations
- Start with subset of type classes
- Use topological sort for modules
- Profile and optimize hot paths
- Allow inline Elixir for FFI
