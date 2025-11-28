# Nova Lang Roadmap

A long-term development roadmap for the Nova compiler project.

---

## Current Status: Bootstrap Complete ✅

**The Nova compiler successfully compiles itself.** Stage 1 (PureScript→JS) and Stage 2 (Elixir) produce identical output for all 8 core modules.

### What's Working
- ~8,300 lines of compiler code across 8 modules
- Full pipeline: Tokenizer → Parser → TypeChecker → CodeGen
- Hindley-Milner type inference with unification
- 25+ expression types, 10 pattern types, all declaration forms
- Elixir/BEAM code generation
- Recursive functions, pattern matching, do-notation
- Operator sections, guards, where clauses

### Architecture
```
Nova Source → Tokenizer → Parser → TypeChecker → CodeGen → Elixir/BEAM
                 ↓          ↓          ↓            ↓
             [Token]     [AST]   [Typed AST]    [String]
```

---

## Phase 1: Language Completeness (Current)

### 1.1 Type Class Resolution 🔶
Complete the type class system for proper method dispatch.

- [ ] Implement constraint solving in type checker
- [ ] Generate dictionary-passing style code
- [ ] Support multi-parameter type classes
- [ ] Functional dependencies
- [ ] Default method implementations

**Impact**: Enables idiomatic `Functor`, `Monad`, `Show`, `Eq` usage

### 1.2 Module System Improvements
- [x] Basic imports/exports
- [ ] Qualified imports with aliases (`import Data.Map as M`)
- [ ] Re-exports
- [ ] Orphan instance detection

### 1.3 Error Messages
- [ ] Source location in all error messages
- [ ] Contextual errors with code snippets
- [ ] Type error explanations (expected vs actual)
- [ ] Suggestions for common mistakes

### 1.4 Pattern Matching Completeness
- [x] All pattern types working
- [ ] Exhaustiveness checking
- [ ] Redundant pattern warnings

---

## Phase 2: Performance & Tooling (Near-term)

### 2.1 Compilation Performance
- [ ] Incremental compilation
- [ ] Parallel module compilation
- [ ] Type checking result caching
- [ ] Native tokenizer (consider Rust/Zig)

### 2.2 Runtime Performance
Current codegen is reasonable but has room for improvement:

- [ ] Arity analysis for lambda optimization
- [ ] Strictness analysis
- [ ] Unboxing for primitive types
- [ ] Specialization for monomorphic sites

**Note**: BEAM already handles tail calls and constant folding.

### 2.3 Developer Tooling
- [ ] Language Server Protocol (LSP)
  - Go to definition
  - Find references
  - Hover type info
  - Completion
- [ ] REPL with type display
- [ ] Source maps for debugging
- [ ] Code formatter

### 2.4 Package Manager
- [ ] Package manifest (nova.toml)
- [ ] Dependency resolution
- [ ] Registry integration
- [ ] Semantic versioning

---

## Phase 3: Advanced Type Features (Medium-term)

### 3.1 Type System Extensions
- [x] Higher-kinded types (partial)
- [ ] Type families / Associated types
- [ ] GADTs
- [ ] Rank-N types
- [ ] Existential types

### 3.2 Row Types & Records
- [x] Basic records
- [ ] Extensible records with row polymorphism
- [ ] Record concatenation/restriction
- [ ] Variant types (extensible sums)

### 3.3 Effect System (Optional)
- [ ] Algebraic effects
- [ ] Effect inference
- [ ] Effect handlers

---

## Phase 4: Alternative Backends (Long-term)

### 4.1 JavaScript Backend
Most practical for web deployment.

```
Nova Source → ... → CodeGenJS → JavaScript
```

- [ ] `CodeGenJS.purs` module
- [ ] JS runtime library
- [ ] JS FFI
- [ ] Source maps
- [ ] Tree shaking
- [ ] Bundle optimization

**Use cases**: Browser apps, Node.js, React/Vue integration

### 4.2 WebAssembly Backend
For performance-critical browser applications.

**Approach Options**:

| Option | Complexity | Performance | Effort |
|--------|------------|-------------|--------|
| Via JS + wasm-pack | Low | Medium | Low |
| Direct WAT/WASM | High | High | High |
| Via Grain/AssemblyScript | Medium | Medium | Medium |

- [ ] Choose approach
- [ ] Implement CodeGenWasm
- [ ] Memory management strategy
- [ ] WASM runtime

### 4.3 Native Backend (LLVM)
For systems programming.

- [ ] CodeGenLLVM module
- [ ] Memory management (RC/GC)
- [ ] C FFI
- [ ] Platform optimizations

### 4.4 Other Potential Targets
- **Lua** - Embedded scripting (games, nginx)
- **Go** - Cloud infrastructure
- **Rust** - Systems with safety
- **JVM** - Enterprise ecosystem

---

## Phase 5: Ecosystem (Long-term)

### 5.1 Standard Library
- [ ] Core data structures (List, Map, Set, Vector)
- [ ] Text processing (Unicode strings)
- [ ] Numeric types (BigInt, Decimal)
- [ ] Date/Time
- [ ] JSON
- [ ] HTTP client
- [ ] File system
- [ ] Concurrency

### 5.2 Testing Framework
- [ ] Property-based testing (QuickCheck)
- [ ] Unit testing
- [ ] Test runner
- [ ] Coverage

### 5.3 Documentation
- [ ] Doc generator (Haddock-style)
- [ ] Interactive examples
- [ ] Tutorials

### 5.4 Build System
- [ ] Declarative config
- [ ] Multi-target builds
- [ ] Conditional compilation
- [ ] Build caching

---

## Version Milestones

### v0.1 - Bootstrap ✅ DONE
- [x] Self-hosting compiler
- [x] Basic type inference
- [x] Elixir code generation
- [x] Core language features

### v0.2 - Usability
- [ ] Good error messages
- [ ] Complete module system
- [ ] Formatter, REPL

### v0.3 - Type Classes
- [ ] Full type class resolution
- [ ] Dictionary passing codegen
- [ ] Standard type class hierarchy

### v0.4 - Performance
- [ ] Incremental compilation
- [ ] Benchmark suite
- [ ] Optimization passes

### v0.5 - Multi-Target
- [ ] JavaScript backend
- [ ] Cross-compilation

### v1.0 - Production Ready
- [ ] Stable specification
- [ ] Comprehensive stdlib
- [ ] Production tooling
- [ ] Documentation

---

## Research Areas

### Language Features to Investigate
- **Dependent types** - Types depending on values
- **Linear types** - Resource management
- **Refinement types** - Types with predicates
- **Gradual typing** - Mixed static/dynamic
- **Metaprogramming** - Compile-time codegen
- **Hot reloading** - BEAM capability

### Interop Priorities
1. **Elixir/Erlang** - First-class OTP
2. **JavaScript** - Browser/Node.js
3. **C** - System access

---

## Design Principles

1. **Simplicity** - Simple solutions over clever ones
2. **Correctness** - Type safety and sound semantics
3. **Performance** - Efficient generated code
4. **Pragmatism** - Real-world usability
5. **Interop** - First-class FFI

---

## Contributing

High-value contribution areas:

1. **Error messages** - User experience
2. **Testing** - Expand test suite
3. **Documentation** - Tutorials, guides
4. **Standard library** - Core utilities
5. **Tooling** - Editor integrations

---

## References

- [PureScript](https://www.purescript.org/) - Language inspiration
- [Haskell](https://www.haskell.org/) - Type system foundation
- [Elixir](https://elixir-lang.org/) - Primary target
- [Erlang/OTP](https://www.erlang.org/) - Runtime platform
- [Elm](https://elm-lang.org/) - Error message inspiration
