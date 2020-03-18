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
unit = lvar +++ paren +++ definition

lvar :: Parser LTerm
lvar = do {v <- token letter; return (LVar [v])}

paren :: Parser LTerm
paren = do {token $ char '('; term <- lterm; token $ char ')'; return term}

definition :: Parser LTerm
definition = do char '$'
                name <- token $ many1 letter
                return $ fst $ head $ parseTerm $ case name of
                    "fix" -> "λf.(λx.f (x x))(λx.f(x x))"
                    "omega" -> "(λx.x x)(λy.y y)"
                    "times" -> "λm.λn.m($plus n) $zero"
                    "plus" -> "λm.λn.λs.λz.m s(n s z)"
                    "succ" -> "λn.λs.λz.s(n s z)"
                    "zero" -> "λs.λz.z"
                    "snd" -> "λp.p $false"
                    "fst" -> "λp.p $true"
                    "pair" -> "λf.λs.λb.b f s"
                    "if" -> "λb.λt.λf.b t f"
                    "false" -> "λt.λf.f"
                    "true" -> "λt.λf.t"
                    "id" -> "λx.x"
                    _ -> error "Not in standard Library"
