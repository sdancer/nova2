# Test topoSortBinds function (lines 147-170 from CodeGenCoreErlang.purs)

test = """
module Test where

import Data.Map as Map
import Data.Set as Set
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))

topoSortBinds binds =
  let bindMap = Map.fromFoldable (Array.mapMaybe (\\b -> getPatternVarName b.pattern # map (\\n -> Tuple n b)) binds)
      bindNames = Set.fromFoldable (Map.keys bindMap)
      deps = map (\\b ->
        let name = fromMaybe "" (getPatternVarName b.pattern)
            freeVars = freeVarsInExprFor bindNames Set.empty b.value
        in { name: name, deps: freeVars }) binds
  in binds
"""

tokens = Nova.Compiler.Tokenizer.tokenize(test)
IO.puts("Tokens: #{length(tokens)}")

case Nova.Compiler.Parser.parse_module(tokens) do
  {:left, err} -> IO.puts("FAIL: #{inspect(err)}")
  {:right, {:tuple, mod, rest}} -> IO.puts("OK: #{length(mod.declarations)} decls, #{length(rest)} rest")
end
