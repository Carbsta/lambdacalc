module Conversion where

import AST
import Lambda
import Data.List
import Parser

import Data.Maybe (fromJust)

stdlib = link <$> parseFileName parseFile "StandardLibrary.lam"     
plus = fromJust <$> (lookup "plus") <$> stdlib

type NamingContext = [String]

removeNames :: NamingContext -> LTerm -> Lambda
removeNames g (LVar x) = Var $ toIndex $ head (elemIndices x g)
removeNames g (LAbs x t) = Abs $ removeNames (x:g) t
removeNames g (LApp t1 t2) = App (removeNames g t1) (removeNames g t2)

--restoreNames :: Lambda -> LTerm
