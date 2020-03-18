module Parser where

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

import AST
import Data.Char
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok

import System.Exit
import System.IO

-- parseFileName :: Parser a -> String -> IO a
-- parseFileName p fileName = parseFromFile (contents p) fileName >>= either report return
--   where
--     report err = do
--         hPutStrLn stderr $ "Error: " ++ show err
--         exitFailure
--
-- parseFile :: Parser [(String, LTerm)]
-- parseFile = endBy parseLine eol
--
-- link :: [(String, LTerm)] -> [(String, LTerm)]
-- link [] = []
-- link ((s,t):ts) = (s, sublink t ts):link ts
--
-- sublink :: LTerm -> [(String, LTerm)] -> LTerm
-- sublink o@(LVar n) ts = case lookup n ts of
--                         Nothing -> o
--                         (Just t) -> t
-- sublink (LAbs _ t) ts = sublink t ts
-- sublink (LApp t u) ts = LApp (sublink t ts) (sublink u ts)
--
-- eol :: Parser String
-- eol =   try (string "\n\r")
--     <|> try (string "\r\n")
--     <|> string "\n"
--     <|> string "\r"
--     <?> "end of line"
--
-- parseLine :: Parser (String, LTerm)
-- parseLine = do
--                 i <- identifier
--                 reservedOp "="
--                 t <- lapp
--                 return (i,t)
--
-- parseTerm :: String -> Either ParseError LTerm
-- parseTerm input = parse (contents lapp) "<stdin>" input

-- lapp :: Parser LTerm
-- lapp = do
--           t1 <- lterm
--           t2 <- lterm
--           return (LApp t1 t2)

strparse :: String -> LTerm
strparse s = case (parse lterm "" s) of
    Left m -> error $ show m
    Right t -> t

lterm :: Parser LTerm
lterm = labs <|> lapp <|> unit

labs :: Parser LTerm
labs = do char 'λ'
          var <- letter
          char '.'
          body  <- lterm
          return (LAbs [var] body)

lapp :: Parser LTerm
lapp = do {ts <- many1 unit; return (foldl1 LApp ts)}

unit :: Parser LTerm
unit = lvar <|> paren

lvar :: Parser LTerm
lvar = do {v <- identifier; return (LVar v)}

paren :: Parser LTerm
paren = do {char '('; term <- lterm; char ')'; return term}

-- lexer

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser emptyDef {
                         Tok.reservedOpNames = ["λ",".","="],
                         Tok.reservedNames   = [],
                         Tok.commentStart    = "--",
                         Tok.commentEnd      = "--",
                         Tok.commentLine     = "",
                         Tok.identStart      = letter,
                         Tok.identLetter     = letter,
                         Tok.opStart         = oneOf ".λ=",
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
