module Evaluator where

import           AST
import           Data.List
import           Parser

-- Free Variables:
-- A variable x is bound to an abstraction when it occurs in the
-- body t of an abstraction \x.t
-- x is free if it appears in a position where it is not bound
-- by an enclosing abstraction on x.
-- FV() in TAPL 5.3.2
fv :: LTerm -> [Name]
fv (LVar x)     = [x]
fv (LAbs x t)   = fv(t) \\ [x]
fv (LApp t1 t2) = fv(t1) ++ fv(t2)

-- Capture Avoiding Subsitution:
-- Notation [x => e2]e1 substitution of all free occurences of an identifier
-- x with an expression e2 in an expression e1.
-- [x => s]x       = s
-- [x => s]y       = y                      if y /= x
-- [x => s](t1 t2) = [x => s]t1 [x => s]t2
-- [x => s](\y.t1) = \y.t1                  if x == y
-- [x => s](\y.t1) = \y.[x => s]t1          if y /= x and y /<- FV(s)
-- [x => s](\y.t1) = \z.[x => s]([y => z]t1)    otherwise
-- where z /= x, z /= y and z /<- FV(s) U FV(t1)
-- Hudak 1.1.1

-- subst x t1 t2 == [x => t1]t2
subst :: Name -> LTerm -> LTerm -> LTerm
subst x t1 t2@(LVar v) | x == v     = t1
                       | otherwise  = t2
subst x t1 (LApp t1' t2') = LApp (subst x t1 t1') (subst x t1 t2')
subst x t1 t2@(LAbs v t) | v == x = t2
                         | notElem v (fv t1) = LAbs v (subst x t1 t)
                         | otherwise = LAbs v' (subst x t1 t')
                                       where
                                               v' = fresh $ (fv t1) ++ (fv t) ++ [x,v]
                                               t' = subst v (LVar v') t

-- pretty lazy way to generate fresh names.
fresh :: [Name] -> Name
fresh ns = (maximum ns)++"'"


-- Evaluation (TAPL 5.3.2):
--   t1 -> t1' / t1 t2 -> t1' t2 (E-App1)
--   t2 -> t2' / v1 t2 -> v1 t2' (E-App2)
-- (\x.t12) v2 / [x => v2]t12    (E-AppAbs)
evalStep :: LTerm -> Either LTerm LTerm
evalStep (LApp (LAbs x t) v2@(LAbs _ _)) = Right $ subst x v2 t
evalStep (LApp v1@(LAbs _ _) t2)         = case evalStep t2 of
                                              Right t2' -> Right $ LApp v1 t2'
                                              Left  t2' -> Left t2'
evalStep (LApp t1 t2)                    = case evalStep t1 of
                                              Right t1' -> Right $ LApp t1' t2
                                              Left  t1' -> Left t1'
evalStep x                               = Left x

eval :: LTerm -> LTerm
eval t = case evalStep t of
            Left  x -> x
            Right x -> eval x
