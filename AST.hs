module AST where

data LTerm = LVar Name
           | LAbs Name LTerm
           | LApp LTerm LTerm
           deriving (Eq, Show)

type Name = String

-- t ::=
--       x
--       \x.t
--       t t
