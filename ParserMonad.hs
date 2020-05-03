module ParserMonad where

import           Control.Applicative
import           Control.Monad
import           Data.Char

-- Parser built following the paper Monadic Parsing in Haskell
-- Graham Hutton & Erik Meijer
-- http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf

newtype Parser a = Parser (String -> [(a, String)])

instance Monad Parser where
        -- return :: a -> Parser a
        return a = Parser (\cs -> [(a,cs)])
        -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
        p >>= f  = Parser (\cs -> concat [parse (f a) cs' |
                                    (a,cs') <- parse p cs])

-- required by Haskell to define functor and applicative instances

instance Functor Parser where
        -- fmap :: (a -> b) -> Parser a -> Parser b
        fmap f p = Parser (\cs -> [(f a, cs')|
                                    (a,cs') <- parse p cs])

instance Applicative Parser where
        -- pure :: a -> Parser a
        pure = return
        -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
        pf <*> pa = Parser (\cs -> [(f a, cs'')|
                                      (f,cs')   <- parse pf cs,
                                      (a,cs'')  <- parse pa cs'])

parse (Parser p) = p

-- consume one character
item :: Parser Char
item =  Parser (\cs -> case cs of
                      ""     -> []
                      (c:cs) -> [(c,cs)])

-- updated from MonadZero and MonadPlus
-- to Alternative

instance Alternative Parser where
        empty   = Parser (\cs -> [])
        p <|> q = Parser (\cs -> parse p cs <|> parse q cs)
        some p = do {a <- p; as <- many p; return (a:as)}
        many p  = some p +++ return []

-- deterministic choice operator that returns at most one result
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p <|> q) cs of
                            []     -> []
                            (x:xs) -> [x])

-- all laws given for (++) hold for (+++)
-- left distribution law precondition met

-- sat takes a predicate and returns a parser that consumes a
-- single character if it satisfies the predicate
sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else empty}

-- e.g parser for specific characters:
char :: Char -> Parser Char
char c = sat (c ==)

-- parser for letters
letter :: Parser Char
letter = sat isAlpha

-- parser for digits
digit :: Parser Char
digit = sat isDigit

-- Parse a specific string
string :: String -> Parser String
string ""     = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}

-- parse repeated applications of parser p.
-- zero or more applications:
-- many :: Parser a -> Parser [a]
-- many p = many1 p +++ return []
--
-- -- one or more:
many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)}

-- parse repeated applications of a parser p,
-- seperated by applications of a parser sep,
-- throws away result values of sep.
sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do a <- p
                    as <- many (do {sep; p})
                    return (a:as)

-- Parse repeated applications of parser p,
-- seperated by applications of parser op, whose result value
-- is an operator that is assumed to associate to the left,
-- and which is used to combine the results from the p parsers:
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
                 where
                    rest a = (do f <- op
                                 b <- p
                                 rest (f a b))
                             +++ return a

-- Some useful lexical combinators

-- Parse a string of spaces, tabs, and newlines
space :: Parser String
space = many (sat isSpace)

-- Parse a token with parser
token :: Parser a -> Parser a
token p = do {a <- p; space; return a}

-- Parse a symbolic token
symb :: String -> Parser String
symb cs = token (string cs)

-- Apply a parser p, throwing away any leading space:
apply :: Parser a -> String -> [(a,String)]
apply p = parse (do {space; p})

-- For multiline documents where preserving new lines is important:

-- parsing spaces, but not newlines
lspace :: Parser String
lspace = many $ sat (\c -> all ($ c) [isSpace, (/= '\n'), (/= '\r')])

ltoken :: Parser a -> Parser a
ltoken p = do {a <-p; lspace; return a}

lsymb :: String -> Parser String
lsymb cs = ltoken (string cs)
