module Parser where

import           AST
import           Data.Char
import           ParserMonad
import Control.Applicative


parseFile :: FilePath -> IO [(String,LTerm)]
parseFile fp = fst <$> head <$>
                apply (comments *> parseLine `sepby` eol <* eol) <$> readFile fp

comments :: Parser ()
comments = do symb "--"
              many (sat (/= '-'))
              symb "--"
              return ()

parseLine :: Parser (String,LTerm)
parseLine = do name <- ltoken $ many1 letter
               lsymb "="
               term <- lterm
               return (name, term)

eol :: Parser Char
eol = char '\n' +++ char '\r'

parseTerm :: String -> [(LTerm,String)]
parseTerm s = apply lterm s

lterm :: Parser LTerm
lterm = labs +++ lapp +++ unit

labs :: Parser LTerm
labs = do absOp
          v <- ltoken letter
          lsymb "."
          t <- lterm
          return (LAbs [v] t)

absOp :: Parser String
absOp = lsymb "λ" +++ lsymb "\\"

lapp :: Parser LTerm
lapp = do {ts <- many1 unit; return (foldl1 LApp ts)}

unit :: Parser LTerm
unit = lvar +++ paren +++ number +++ definition

lvar :: Parser LTerm
lvar = do {v <- ltoken letter; return (LVar [v])}

paren :: Parser LTerm
paren = do {lsymb "("; term <- lterm; lsymb ")"; return term}

number :: Parser LTerm
number = do {n <- ltoken $ many1 digit; return $ LAbs "n" (LAbs "m" (times $ read n))}
        where times 0 = LVar "m"
              times m = LApp (LVar "n") (times $ m-1)


definition :: Parser LTerm
definition = do char '$'
                name <- ltoken $ many1 letter
                return $ fst $ head $ parseTerm $ case name of
                    "id" -> "λx.x"
                    "succ" -> "λn.λs.λz.s(n s z)"
                    "plus" -> "λm.λn.λf.λz.m s(n f z)"
                    "mult" -> "λm.λn.λf.m (n f)"
                    "pow" -> "λb.λe.e b"
                    "pred" -> "λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)"
                    "sub" -> "λm.λn.n $pred m"
                    "true" -> "λt.λf.t"
                    "false" -> "λt.λf.f"
                    "and" -> "λp.λq.p q p"
                    "or" -> "λp.λq.p p q"
                    "not" -> "λp.p $false $true"
                    "if" -> "λb.λt.λf.b t f"
                    "iszero" -> "λn.n (λx.$false) $true"
                    "leq" -> "λm.λn.$iszero ($sub m n)"
                    "eq" -> "λm.λn.$and ($leq m n) ($leq n m)"
                    "pair" -> "λf.λs.λb.b f s"
                    "fst" -> "λp.p $true"
                    "snd" -> "λp.p $false"
                    "nil" -> "λx.$true"
                    "null" -> "λp.p (λx.λy.$false)"
                    "shift" -> "λx.$pair ($second x) ($succ ($second x))"
                    "fix" -> "λf.(λx.f (x x))(λx.f(x x))"
                    "omega" -> "(λx.x x)(λy.y y)"
                    _ -> error "Not in standard Library"
