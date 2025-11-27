module Test.Parser.TraceTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Nova.Compiler.Tokenizer (tokenize, TokenType(..))
import Nova.Compiler.Parser as P

main :: Effect Unit
main = do
  log "=== Trace Test ==="

  let input = """tTuple ts = TyCon { name: "Tuple" <> show (length ts), args: ts }
  where
    length arr = 0"""
  let tokens = tokenize input
  log $ "Input tokens: " <> show (Array.take 10 (map _.value tokens))

  -- Step 1: parse identifier name
  log "Step 1: parse identifier name"
  case P.parseIdentifierName tokens of
    Right (Tuple name rest) -> do
      log $ "  ✓ Got name: " <> name
      log $ "  Remaining: " <> show (Array.take 10 (map _.value rest))

      -- Step 2: parse patterns
      log "Step 2: parse patterns"
      case P.parseMany P.parseSimplePattern rest of
        Right (Tuple pats rest') -> do
          log $ "  ✓ Got " <> show (Array.length pats) <> " patterns"
          log $ "  Remaining: " <> show (Array.take 10 (map _.value rest'))

          -- Step 3: expect =
          log "Step 3: expect ="
          case P.expectOperator rest' "=" of
            Right (Tuple _ rest'') -> do
              log $ "  ✓ Got ="
              log $ "  Remaining: " <> show (Array.take 10 (map _.value rest''))

              -- Step 4: parse expression
              log "Step 4: parse expression"
              case P.parseExpression rest'' of
                Right (Tuple _ rest''') -> do
                  log $ "  ✓ Got expression"
                  log $ "  Remaining: " <> show (Array.take 10 (map _.value rest'''))
                Left err -> do
                  log $ "  ✗ Error: " <> err
                  -- Try parsing term instead
                  log "  Trying parseTerm..."
                  case P.parseTerm rest'' of
                    Right (Tuple _ rest''') ->
                      log $ "  parseTerm OK, remaining: " <> show (Array.take 10 (map _.value rest'''))
                    Left err2 ->
                      log $ "  parseTerm failed: " <> err2

            Left err -> log $ "  ✗ Error: " <> err

        Left err -> log $ "  ✗ Error: " <> err
    Left err -> log $ "  ✗ Error: " <> err

  log "=== Done ==="
