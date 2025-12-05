module Nova.Compiler.CstPipeline where

import Prelude
import Data.List (List)
import Data.Either (Either)
import Data.Tuple (Tuple(..), fst)
import Data.Void (Void)
import Nova.Compiler.Cst as Cst
import Nova.Compiler.CstLexer as Lexer
import Nova.Compiler.CstParser as Parser
import Nova.Compiler.CstToAst as CstToAst
import Nova.Compiler.Ast as Ast

-- | Parse source code using the CST pipeline and convert to AST
parseModuleCst :: String -> Either String Ast.Module
parseModuleCst source = do
  let tokens = Lexer.lexModule source
  cstResult <- Parser.runParser Parser.parseModule tokens
  let cstMod = fst cstResult
  CstToAst.convertModule cstMod

-- | Parse source code to CST only (for debugging)
parseModuleToCst :: String -> Either String (Cst.Module Void)
parseModuleToCst source = do
  let tokens = Lexer.lexModule source
  cstResult <- Parser.runParser Parser.parseModule tokens
  pure (fst cstResult)

-- | Lex source code to tokens (for debugging)
lexSource :: String -> List Cst.SourceToken
lexSource = Lexer.lexModule
