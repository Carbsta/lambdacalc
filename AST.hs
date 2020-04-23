module AST where

data LTerm = LVar Name
           | LAbs Name LTerm
           | LApp LTerm LTerm
           deriving (Ord, Eq)

type Name = String

instance Show LTerm where
    showsPrec _ (LVar n) = shows n
    showsPrec _ (LAbs n l) = (:) 'λ' . shows n . (:) '.' . shows l
    showsPrec _ (LApp l r) = (:) '(' . shows l . (:) ')'
        .  (:) '(' . shows r . (:) ')'

-- t ::=
--       x
--       \x.t
--       t t
