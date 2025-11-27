module Test.Parser.TestDropInstance2 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Nova.Compiler.Tokenizer (tokenize, Token, TokenType(..))
import Nova.Compiler.Parser as P

main :: Effect Unit
main = do
  log "=== Drop Instance Test 2 ==="
  
  let input = """dropInstanceConstraints :: Array Token -> Array Token
dropInstanceConstraints tokens =
  -- Look for => which separates constraints from the instance head
  -- e.g., instance (Show a, Eq a) => MyClass a where ...
  -- We only look at the beginning of the token stream, not deep into the file
  let tokens' = skipNewlines tokens
      -- Check if we have constraints by looking for =>
      { init: before, rest: after } = Array.span (\t -> not (t.tokenType == TokOperator && t.value == \"=>\")) tokens'
  in case Array.head after of
    -- Only skip if we actually found => at a reasonable position (within first ~20 tokens)
    Just t | t.tokenType == TokOperator, t.value == \"=>\", Array.length before < 20 ->
      skipNewlines (Array.drop 1 after)
    _ -> tokens'"""
  let tokens = tokenize input
  log $ "Tokens: " <> show (Array.length tokens)
  
  log ""
  log "parseDeclaration:"
  case P.parseDeclaration tokens of
    Right (Tuple decl rest) -> do
      log "✓ parsed"
      let rest' = P.skipNewlines rest
      log $ "Rest: " <> show (Array.length rest') <> " tokens"
      log $ "Rest first: " <> show (map showTok (Array.take 5 rest'))
    Left err -> log $ "✗ " <> err

showTok :: Token -> String
showTok t = t.value <> ":L" <> show t.line <> ":" <> showTokType t.tokenType

showTokType :: TokenType -> String
showTokType TokKeyword = "K"
showTokType TokIdentifier = "I"
showTokType TokOperator = "O"
showTokType TokDelimiter = "D"
showTokType TokNewline = "NL"
showTokType TokNumber = "N"
showTokType TokString = "S"
showTokType TokChar = "C"
showTokType TokUnrecognized = "?"

