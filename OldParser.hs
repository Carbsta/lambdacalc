module OldParser where

import           AST
import           Data.Char
import           ParserMonad

parser :: String -> [(LTerm,String)]
parser s = apply lterm s

lterm :: Parser LTerm
lterm = lapp +++ llvar +++ labs

llvar :: Parser LTerm
llvar = do {v <- name; return (LVar v)}

name :: Parser Name
name = do {v <- token (sat isLetter); return v}

labs :: Parser LTerm
labs = do symb "\\"
          v <- lvar
          symb "."
          t <- lterm
          return (LAbs v t)

lapp :: Parser LTerm
lapp = do {t1 <- lterm; t2 <- lterm; return (LApp t1 t2)}
