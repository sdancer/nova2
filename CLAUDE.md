# CLAUDE.md

Nova Lang: PureScript compiler → Elixir. Self-hosted on BEAM VM.

## Critical Rules

**Every warning is wasted tokens = wasted money. Fix all warnings.**

**Root cause only.** Never patch symptoms. Fix the source:
- Bad generated code → fix `CodeGen.purs`
- Missing runtime → add real implementation
- Test failure → fix compiler, not test infrastructure

**No workarounds.** Never add string replacements, regex patches, or injected code to make tests pass.

**Validate before changing.** Test theory with minimal case first. If `npx spago build` accepts it, our compiler should too.

## Commands

```bash
npx spago build                    # Build compiler
npx spago test                     # Run tests
node scripts/regenerate-purs.js    # Regenerate Elixir output
cd nova_lang && mix test           # Test compiled output
cd nova_lang && mix run test_self_host_all.exs  # Self-host verification
```

## Workflow After Changes

1. `npx spago build` — must pass, zero warnings
2. `npx spago test` — must pass
3. `node scripts/regenerate-purs.js` — must succeed (compiler compiles itself)
4. `cd nova_lang && mix test` — must pass

**Self-hosting is the ultimate test.** Output must be byte-for-byte identical. "EXACT MATCH" or "whitespace diff only" = good. Any semantic diff = bug.

**See [SOURCESTRUCTURE.md](SOURCESTRUCTURE.md)** for directory layout, modules, and types. Update it when adding/changing modules.

## Don't Do This

- **Don't edit `nova_lang/` directly** — it's generated output
- **Don't skip regeneration** — always verify self-hosting works after changes
- **Don't add test workarounds** — fix the compiler instead
- **Don't ignore warnings** — each one costs tokens on every future interaction
- **Don't make changes without a theory** — validate with minimal test first
- **Don't patch output** — if CodeGen produces wrong code, fix CodeGen

## Parser/Tokenizer Changes

1. **Test first**: Add failing test to `test/parser/` before fixing
2. **Run `npx spago test`** after any Parser.purs or Tokenizer.purs change
3. Verify new test passes

Parser pattern: `Either String (Tuple a (Array Token))` with helpers `skipNewlines`, `expectKeyword`, `expectDelimiter`.

## Current Focus

Removing hardcoded scaffolding, generalizing compiler core modules. Self-hosting works; now refining.
