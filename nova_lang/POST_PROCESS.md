# Post-Processing Steps for Generated Elixir Code

After running `node scripts/regenerate.js`, apply these manual optimizations:

## Tokenizer.ex - Charlist Optimization

The generated Tokenizer.ex uses `String.at(s, n)` which is O(n) in Elixir due to UTF-8 scanning. This makes tokenization O(n²) for large files.

### Fix: Use charlist with O(1) head access

1. **Update `init_state`** - Add charlist to state:
```elixir
def init_state(input) do
  %{input: input, chars: String.to_charlist(input), pos: 0, line: 1, column: 1}
end
```

2. **Update `peek`** - Use pattern matching (O(1)):
```elixir
def peek(state) do
  case state.chars do
    [] -> :nothing
    [c | _] -> {:just, c}
  end
end
```

3. **Update `peek_at`** - Use Enum.at on charlist:
```elixir
def peek_at(state, offset) do
  case Enum.at(state.chars, offset) do
    nil -> :nothing
    c -> {:just, c}
  end
end
```

4. **Update `advance`** - Drop from charlist head (O(1)):
```elixir
def advance(state, 1) do
  %{state | pos: state.pos + 1, column: state.column + 1, chars: tl(state.chars)}
end
def advance(state, 2) do
  %{state | pos: state.pos + 2, column: state.column + 2, chars: state.chars |> tl() |> tl()}
end
def advance(state, 3) do
  %{state | pos: state.pos + 3, column: state.column + 3, chars: state.chars |> tl() |> tl() |> tl()}
end
def advance(state, n) do
  %{state | pos: state.pos + n, column: state.column + n, chars: Enum.drop(state.chars, n)}
end
```

5. **Update `advance_tab`** - Drop from charlist:
```elixir
def advance_tab(state) do
  next_col = (state.column + (8 - rem((state.column - 1), 8)))
  %{state | pos: (state.pos + 1), column: next_col, chars: tl(state.chars)}
end
```

6. **Update `tokenize_operator`** - Use charlist prefix instead of O(n) string slice:
```elixir
def tokenize_operator(state) do
  start_line = state.line
  start_col = state.column
  start_pos = state.pos
  # Only take first 3 chars from charlist (max operator length)
  prefix = state.chars |> Enum.take(3) |> List.to_string()
  tok_type = :tok_operator
  op = find_operator(prefix, operators())
  len = String.length(op)
  {:just, {:tuple, mk_token(tok_type, op, start_line, start_col, start_pos), advance(state, len)}}
end
```

### Performance Impact

| File | Before | After | Speedup |
|------|--------|-------|---------|
| Types.purs | 101,468ms | 15.9ms | 6,382x |
| Parser.purs | 16,488ms | 103.8ms | 159x |
| Total | ~130sec | ~400ms | 325x |

### Why This Is Needed

- PureScript's `String.charAt` is O(1) in JavaScript
- Elixir's `String.at` is O(n) due to UTF-8 encoding
- Elixir linked lists have O(1) head/tail access
- This optimization cannot be expressed in PureScript source

## Type Checker Performance Analysis

### Current Performance (after Tokenizer fix)

| Phase | Time | % |
|-------|------|---|
| Tokenize | ~20ms | 1% |
| Parse | ~180ms | 10% |
| **Type Check** | **~1560ms** | **88%** |
| CodeGen | ~14ms | 1% |

### Type Checker Bottleneck: builtinPrelude

The `builtinPrelude` function in Types.purs takes 1505ms (96.5% of type checking time).

**Why it's slow:**
- The function body contains ~359 entries in a `Map.fromFoldable` call
- Each entry is a `Tuple "name" (mkScheme vars type)` expression
- Type inference processes ~3000+ expressions:
  - Looking up polymorphic `Tuple` constructor
  - Instantiating type schemes with fresh variables
  - Inferring nested `mkScheme` calls
  - Unifying result types

**Breakdown:**
- `builtinPrelude`: 1505ms (96.5%)
- Other 85 functions: ~55ms (3.5%)

### Potential Optimizations

1. **Skip type checking builtinPrelude** - It's trusted hardcoded data, not user code
2. **Memoize repeated type inference** - `mkScheme [] tInt` appears many times
3. **Batch similar expressions** - All entries have the same structure

### Current Workaround

No fix applied yet. The type checker performance is acceptable for development but could be improved by:

1. Adding a `@trusted` annotation for known-correct functions
2. Moving `builtinPrelude` to a pre-compiled form that doesn't need type checking
3. Caching inference results for common subexpressions
