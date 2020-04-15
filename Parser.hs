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
absOp = lsymb "λ" +++ symb "\\"

lapp :: Parser LTerm
lapp = do {ts <- many1 unit; return (foldl1 LApp ts)}

unit :: Parser LTerm
unit = lvar +++ paren +++ definition

lvar :: Parser LTerm
lvar = do {v <- ltoken letter; return (LVar [v])}

paren :: Parser LTerm
paren = do {lsymb "("; term <- lterm; lsymb ")"; return term}

definition :: Parser LTerm
definition = do char '$'
                name <- ltoken $ many1 letter
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
