module Evaluator where

import           AST
import           Data.Either
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
-- [x => s]x       = s
-- [x => s]y       = y                      if y /= x
-- [x => s](\y.t1) = \y.[x -> s]t1          if y /= x and y /<- FV(s)
-- [x => s](t1 t2) = [x => s]t1 [x => s]t2
-- Note, partial, requires alpha conversion.
-- subst x t1 t2 == [x => t1]t2
subst :: Name -> LTerm -> LTerm -> LTerm
subst x t1 t2@(LVar v) | x == v     = t1
                       | otherwise  = t2
subst x t1 (LAbs v t) | v /= x && notElem v (fv t1) = LAbs v (subst x t1 t)
                      | otherwise                   = error $ "Cannot substitute '" ++ show x ++ "' with '" ++ show t1 ++ "' in term '" ++ show (LAbs v t) ++ "'"
subst x t1 (LApp t1' t2') = LApp (subst x t1 t1') (subst x t1 t2')


-- Evaluation (TAPL 5.3.2):
--   t1 -> t1' / t1 t2 -> t1' t2 (E-App1)
--   t2 -> t2' / v1 t2 -> v1 t2' (E-App2)
-- (\x.t12) v2 / [x => v2]t12    (E-AppAbs)



tester :: String -> LTerm
tester s = fromRight (LVar "parseError") (parseTerm s)
