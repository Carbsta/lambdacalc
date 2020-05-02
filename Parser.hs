module Parser where

import           Lambda
import           Data.Char
import           ParserMonad
import Control.Applicative


parseFile :: FilePath -> IO [(String,Lambda)]
parseFile fp = fst <$> head <$>
                apply (comments *> parseLine `sepby` eol <* eol) <$> readFile fp

comments :: Parser ()
comments = do symb "--"
              many (sat (/= '-'))
              symb "--"
              return ()

parseLine :: Parser (String,Lambda)
parseLine = do name <- ltoken $ many1 letter
               lsymb "="
               term <- lterm
               return (name, term)

eol :: Parser Char
eol = char '\n' +++ char '\r'

parseTerm :: String -> [(Lambda,String)]
parseTerm s = apply lterm s

lterm :: Parser Lambda
lterm = labs +++ lapp +++ unit

labs :: Parser Lambda
labs = do absOp
          v <- ltoken letter
          lsymb "."
          t <- lterm
          return (Abs [v] t)

absOp :: Parser String
absOp = lsymb "λ" +++ lsymb "\\"

lapp :: Parser Lambda
lapp = do {ts <- many1 unit; return (foldl1 App ts)}

unit :: Parser Lambda
unit = lvar +++ paren +++ number +++ definition

lvar :: Parser Lambda
lvar = do {v <- ltoken letter; return (Var [v])}

paren :: Parser Lambda
paren = do {lsymb "("; term <- lterm; lsymb ")"; return term}

number :: Parser Lambda
number = do {n <- ltoken $ many1 digit; return $ Abs "n" (Abs "m" (times $ read n))}
        where times 0 = Var "m"
              times m = App (Var "n") (times $ m-1)


definition :: Parser Lambda
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
                    "fac" -> "λn.λf.n(λf.λn.n(f(λf.λx.n f(f x))))(λx.f)(λx.x)"
                    "fib" -> "λn.λf.n(λc.λa.λb.c b(λx.a (b x)))(λx.λy.x)(λx.x)f"
                    "omega" -> "(λx.x x)(λy.y y)"
                    _ -> error "Not in standard Library"
