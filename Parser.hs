module Parser where

import AST
import Data.Char
import Text.Parsec
import Text.Parsec.String (Parser)

import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok

parseTerm :: String -> Either ParseError LTerm
parseTerm input = parse (contents lapp) "<stdin>" input

lapp :: Parser LTerm
lapp = do {ts <- many1 lterm; return (foldl1 LApp ts)}

lterm :: Parser LTerm
lterm = parens lapp
        <|> lvar
        <|> labs

lvar :: Parser LTerm
lvar = do {v <- identifier; return (LVar v)}

labs :: Parser LTerm
labs = do reservedOp "\\"
          v <- identifier
          reservedOp "."
          t  <- lapp
          return (LAbs v t)

-- lexer

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser emptyDef {
                         Tok.reservedOpNames = ["\\","."],
                         Tok.reservedNames   = [],
                         Tok.identStart      = letter,
                         Tok.identLetter     = satisfy (const False),
                         Tok.opStart         = oneOf ".\\",
                         Tok.opLetter        = satisfy (const False)
                         -- identifiers and operators are single characters
                                   }

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

contents :: Parser a -> Parser a
contents p = do Tok.whiteSpace lexer
                result <- p
                eof
                return result
