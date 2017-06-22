{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module STLC where

import Language.LBNF.Compiletime
import Language.LBNF (lbnf, bnfc)

bnfc [lbnf|

Bool. TType ::= "Bool";
-- ^ Bool is the base type.
Arr.  TType ::= TType "->" TType;
-- ^ For any two types t1 and t2, t1 -> t2 is also a type.

ValTrue.  BValue ::= "true";
ValFalse. BValue ::= "false";

Ap.  Expr ::= Expr Expr;
Lam. Expr ::= "\\" Ident ":" TType "." Expr;
Var. Expr ::= Ident;
Val. Expr ::= BValue ;

coercions Expr 2 ;

|]

-- The code above generate several things:
-- - A parser named pExpr
-- - A lexer named myLexer
-- They can be used together: pExpr $ myLexer "\\x : Bool. x"
-- - A pretty printer

eval :: Expr -> Expr
eval = undefined

parseTerm :: String -> ParseMonad Expr
parseTerm = pExpr . myLexer

parseAndEvaluate :: String -> String
parseAndEvaluate str
  = case parseTerm str of
      Ok expr -> printTree $ eval expr
      Bad err -> err
