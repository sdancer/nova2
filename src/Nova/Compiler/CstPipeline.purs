module Nova.Compiler.CstPipeline where

import Prelude
import Data.List (List)
import Data.Either (Either)
import Data.Tuple (Tuple(..), fst)
import Data.Void (Void)
import Nova.Compiler.Cst as Cst
import Nova.Compiler.CstLexer (lexModule)
import Nova.Compiler.CstParser (runParser, parseModule)
import Nova.Compiler.CstToAst (convertModule)
import Nova.Compiler.Ast as Ast

-- | Parse source code using the CST pipeline and convert to AST
parseModuleCst :: String -> Either String Ast.Module
parseModuleCst source = do
  let tokens = lexModule source
  cstResult <- runParser parseModule tokens
  let cstMod = fst cstResult
  convertModule cstMod

-- | Parse source code to CST only (for debugging)
parseModuleToCst :: String -> Either String (Cst.Module Void)
parseModuleToCst source = do
  let tokens = lexModule source
  cstResult <- runParser parseModule tokens
  pure (fst cstResult)

-- | Lex source code to tokens (for debugging)
lexSource :: String -> List Cst.SourceToken
lexSource = lexModule
