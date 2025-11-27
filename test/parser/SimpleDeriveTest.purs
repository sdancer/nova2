module Test.Parser.SimpleDeriveTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Nova.Compiler.Tokenizer (tokenize, Token, TokenType(..))
import Nova.Compiler.Parser as P

main :: Effect Unit
main = do
  log "=== Simple Derive Test ==="

  content <- readTextFile UTF8 "src/Nova/Compiler/Tokenizer.purs"
  let allTokens = tokenize content

  let tokens9 = skipDecls allTokens 9
  let toks = P.skipNewlines tokens9

  -- Simulate parseTypeClassInstance EXACTLY as in actual code
  log "Simulating parseTypeClassInstance (exact code path):"

  -- Line 1014-1016
  let Tuple tokens' derived = case Array.head toks of
        Just t | t.tokenType == TokKeyword, t.value == "derive" -> Tuple (Array.drop 1 toks) true
        _ -> Tuple toks false
  log $ "  derived = " <> show derived

  -- Line 1017
  case P.expectKeyword tokens' "instance" of
    Left err -> log $ "  ✗ expectKeyword 'instance' failed: " <> err
    Right (Tuple _ rest) -> do
      log "  ✓ Found 'instance'"

      -- Line 1018
      let rest' = P.dropNewlines rest
      log $ "  rest' first: " <> showTok (unsafeHead rest')

      -- Line 1020-1021
      case Array.head rest' of
        Just t | t.tokenType == TokIdentifier -> do
          log $ "  ✓ First is identifier: " <> t.value

          -- Line 1022-1023
          case Array.head (Array.drop 1 rest') of
            Just t' | t'.tokenType == TokOperator, t'.value == "::" -> do
              log "  ✓ Second is :: operator"

              -- Line 1024 - dropInstanceConstraints NOT dropNewlines!
              let rest'' = dropInstanceConstraints (Array.drop 2 rest')
              log $ "  After dropInstanceConstraints, first 5: " <> show (map showTok (Array.take 5 rest''))

              -- Line 1025
              case P.parseType rest'' of
                Left err -> log $ "  ✗ parseType failed: " <> err
                Right (Tuple ty rest''') -> do
                  log "  ✓ parseType succeeded"
                  log $ "  rest''' first 5: " <> show (map showTok (Array.take 5 rest'''))

                  -- Line 1027
                  case P.expectKeyword rest''' "where" of
                    Right (Tuple _ rest4) -> log "  Found 'where'"
                    Left _ -> do
                      log "  No 'where'"
                      -- Line 1037-1047
                      if derived then
                        log "  → derived=true, should return success"
                      else
                        log "  → derived=false, should fail"
            Just t' -> log $ "  ✗ Second is not ::, it's: " <> showTok t'
            Nothing -> log "  ✗ No second token"
        Just t -> log $ "  First is not identifier: " <> showTok t
        Nothing -> log "  ✗ No first token"

  -- Now try the actual function
  log "\nActual parseTypeClassInstance result:"
  case P.parseTypeClassInstance toks of
    Right (Tuple _ rest) -> log $ "✓ Succeeded, remaining: " <> show (Array.length rest)
    Left err -> log $ "✗ Failed: " <> err

-- Copy of dropInstanceConstraints from Parser.purs
dropInstanceConstraints :: Array Token -> Array Token
dropInstanceConstraints tokens =
  let { rest: after } = Array.span (\t -> not (t.tokenType == TokOperator && t.value == "<=")) tokens
  in case Array.head after of
    Just t | t.tokenType == TokOperator, t.value == "<=" -> Array.drop 1 after
    _ -> tokens

skipDecls :: Array Token -> Int -> Array Token
skipDecls tokens 0 = tokens
skipDecls tokens n =
  let tokens' = P.skipNewlines tokens
  in case P.parseDeclaration tokens' of
    Right (Tuple _ rest) -> skipDecls rest (n - 1)
    Left _ -> tokens'

unsafeHead :: Array Token -> Token
unsafeHead arr = case Array.head arr of
  Just t -> t
  Nothing -> { tokenType: TokUnrecognized, value: "<EMPTY>", line: 0, column: 0, pos: 0 }

showTok :: Token -> String
showTok t = t.value <> ":" <> showTokType t.tokenType

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
