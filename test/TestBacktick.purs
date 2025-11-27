module Test.TestBacktick where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Nova.Compiler.Parser (parseDeclarations)
import Nova.Compiler.Tokenizer (tokenize)
import Nova.Compiler.CodeGen (genModule)
import Nova.Compiler.Ast (Declaration(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array

main :: Effect Unit
main = do
  let src = "module Test where\ntest = c `Array.elem` xs"
  let tokens = tokenize src
  log $ "Testing backtick syntax"
  case parseDeclarations tokens of
    Left err -> log $ "Parse error: " <> err
    Right (Tuple decls _) -> do
      let mod = { name: "Test", declarations: decls }
      let code = genModule mod
      log code
