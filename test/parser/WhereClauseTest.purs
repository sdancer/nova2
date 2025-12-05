module Test.WhereClauseTest where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Nova.Compiler.CstLexer as Lexer
import Nova.Compiler.CstParser as Parser
import Data.List as List

-- Constructor pattern let in do with longer names
testSource :: String
testSource = """module Test where

test :: Int
test = do
  let Tuple items hiding = result
"""

main :: Effect Unit
main = do
  log "Testing nested nullary constructor..."
  let tokens = Lexer.lexModule testSource
  log $ "Token count: " <> show (List.length tokens)
  case Parser.runParser Parser.parseModule tokens of
    Left err -> log $ "Parse error: " <> err
    Right (Tuple mod remaining) -> do
      log $ "Parse success! Declarations: " <> show (List.length mod.body.decls)
      log $ "Remaining tokens: " <> show (List.length remaining)
