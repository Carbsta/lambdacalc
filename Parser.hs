module Parser where

import           AST
import           Data.Char
import           ParserMonad

parseTerm :: String -> [(LTerm,String)]
parseTerm s = apply lterm s

lterm :: Parser LTerm
lterm = labs +++ lapp +++ unit

labs :: Parser LTerm
labs = do symb "λ"
          v <- token letter
          symb "."
          t <- lterm
          return (LAbs [v] t)

lapp :: Parser LTerm
lapp = do {ts <- many1 unit; return (foldl1 LApp ts)}

unit :: Parser LTerm
unit = lvar +++ paren

lvar :: Parser LTerm
lvar = do {v <- token letter; return (LVar [v])}

paren :: Parser LTerm
paren = do {token $ char '('; term <- lterm; token $ char ')'; return term}
