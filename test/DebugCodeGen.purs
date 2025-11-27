module Test.DebugCodeGen where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Nova.Compiler.Tokenizer (tokenize)
import Nova.Compiler.Parser (parseDeclarations)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.Traversable (traverse)
import Nova.Compiler.Ast (Declaration(..), FunctionDeclaration)

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "src/Nova/Compiler/CodeGen.purs"
  let tokens = tokenize content
  case parseDeclarations tokens of
    Left err -> log $ "Parse error: " <> err
    Right (Tuple decls rest) -> do
      log $ "Parsed " <> show (Array.length decls) <> " declarations"
      -- Show last few declarations
      let lastFew = Array.drop (Array.length decls - 3) decls
      _ <- log "Last 3 declarations:"
      _ <- traverse (\d -> case d of
        DeclFunction f -> log $ "  Function: " <> f.name
        DeclTypeSig ts -> log $ "  TypeSig: " <> ts.name
        _ -> log $ "  Other"
        ) lastFew
      case Array.head rest of
        Nothing -> log "No remaining tokens"
        Just t -> log $ "Next token: " <> t.value <> " at line " <> show t.line
