module Lambda where

import Data.List.Split (chunksOf)
import Data.List (elemIndices, (\\))

-- Named representation of lambda terms.
-- t ::=
--        x
--        \x.t
--        t t
-- Names are represented by strings.

data Lambda = Var Name
            | Abs Name Lambda
            | App Lambda Lambda
           deriving (Ord, Eq)

type Name = String

-- Pretty printing
instance Show Lambda where
    showsPrec _ (Var n) = shows n
    showsPrec _ (Abs n l) = (:) 'λ' . shows n . (:) '.' . shows l
    showsPrec _ (App l r) = (:) '(' . shows l . (:) ')'
        .  (:) '(' . shows r . (:) ')'

-- Free Variables:
-- A variable x is bound to an abstraction when it occurs in the
-- body t of an abstraction \x.t
-- x is free if it appears in a position where it is not bound
-- by an enclosing abstraction on x.
-- FV() in TAPL 5.3.2
fv :: Lambda -> [Name]
fv (Var x)     = [x]
fv (Abs x t)   = fv(t) \\ [x]
fv (App t1 t2) = fv(t1) ++ fv(t2)

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
subst :: Name -> Lambda -> Lambda -> Lambda
subst x t1 t2@(Var v) | x == v     = t1
                      | otherwise  = t2
subst x t1 (App t1' t2') = App (subst x t1 t1') (subst x t1 t2')
subst x t1 t2@(Abs v t) | v == x = t2
                        | notElem v (fv t1) = Abs v (subst x t1 t)
                        | otherwise = Abs v' (subst x t1 t')
                                      where
                                              v' = fresh $ (fv t1) ++ (fv t) ++ [x,v]
                                              t' = subst v (Var v') t

-- simple way to generate fresh names.
fresh :: [Name] -> Name
fresh ns = (maximum ns)++"'"


-- Call by Value Evaluation (TAPL 5.3.2):
--   t1 -> t1' / t1 t2 -> t1' t2 (E-App1)
--   t2 -> t2' / v1 t2 -> v1 t2' (E-App2)
-- (\x.t12) v2 / [x => v2]t12    (E-AppAbs)
evalStep :: Lambda -> Either Lambda Lambda
evalStep (App (Abs x t) v2@(Abs _ _)) = Right $ subst x v2 t
evalStep (App v1@(Abs _ _) t2)         = case evalStep t2 of
                                              Right t2' -> Right $ App v1 t2'
                                              Left  t2' -> Left t2'
evalStep (App t1 t2)                    = case evalStep t1 of
                                              Right t1' -> Right $ App t1' t2
                                              Left  t1' -> Left t1'
evalStep x                               = Left x

eval :: Lambda -> Lambda
eval t = case evalStep t of
            Left  x -> x
            Right x -> eval x

-------------------------------------------------------------------------------
{-|
de Bruijn notation of lambda terms, with incidices in a unary representation as
successors of zero. Zero corresponds to the closest binder to a variable,
with the successor S increasing the distance between variable and binder by one.
If a variable index is larger than the number of binders between the term and
root of the expression (e.g the index points to a binder outside of the term)
then that index represents a free variable within that term.
-}

data Index = S Index | Zero

type ScopeLevel = Int

instance Show Index where
    showsPrec _ = shows . toInt

-- Unary representation ensures Indexes are never negative
toIndex :: (Num a, Ord a) => a -> Index
toIndex 0 = Zero
toIndex n | n < 0 = error "Can't have a negative index"
          | otherwise = S $ toIndex (n-1)

toInt :: (Num a) => Index -> a
toInt Zero  = 0
toInt (S i) = 1 + toInt i

data ILambda = IVar Index
             | IAbs ILambda
             | IApp ILambda ILambda

-- Pretty printing
instance Show ILambda where
    showsPrec _ (IVar n) = shows n
    showsPrec _ (IAbs l) = (:) 'λ' . shows l
    showsPrec _ (IApp l r) = (:) '(' . shows l . (:) ')'
        .  (:) '(' . shows r . (:) ')'

-- This determines the maximum depth of binders, which can then be used to
-- determine if a given variable Index points to a free variable or not.
scopeLevel :: (Num a, Ord a) => ILambda -> a
scopeLevel (IVar _)   = 0
scopeLevel (IAbs l)   = 1 + scopeLevel l
scopeLevel (IApp l r) = max (scopeLevel l) (scopeLevel r)

shift :: (Num a, Ord a)=> a -> ILambda -> a -> ILambda
shift d (IVar k) c
    | toInt(k) < c = IVar k
    | otherwise = IVar $ toIndex $ toInt(k) + d
shift d (IAbs l) c = IAbs $ shift d l (c+1)
shift d (IApp l r) c = IApp (shift d l c) (shift d r c)

isubst :: (Num a, Eq a, Ord a) => a -> ILambda -> ILambda -> ILambda
isubst j s (IVar k)
    | toInt(k) == j = s
    | otherwise = IVar k
isubst j s (IAbs l) = IAbs $ isubst (j+1) (shift 1 s 0) l
isubst j s (IApp l r) = IApp (isubst j s l) (isubst j s r)

-- Evaluation:
--   t1 -> t1' / t1 t2 -> t1' t2 (E-App1)
--   t2 -> t2' / v1 t2 -> v1 t2' (E-App2)
-- (\x.t12) v2 -> ^-1[0 => ^1(v2)]t12 (E-AppAbs) - beta reduction
-- TAPL
ieval :: ILambda -> ILambda
ieval (IApp (IAbs l) v@(IAbs _)) = shift (-1) (isubst 0 (shift 1 v 0) l) 0
ieval (IApp v@(IAbs _) t)        = IApp v (ieval t)
ieval (IApp l r)                 = IApp (ieval l) r
ieval x                          = x

-- steppedEval :: Int -> ILambda -> ILambda
-- steppedEval 0 x = x
-- steppedEval _ (IApp (IAbs l) v@(IAbs _)) = shift (-1) (subst 0 (shift 1 v 0) l) 0
-- steppedEval n (IApp v@(IAbs _) t) = IApp v (steppedEval (n-1) t)
-- steppedEval n (IApp l r) = IApp (steppedEval (n-1) l) r
-- steppedEval _ x = x

-------------------------------------------------------------------------------
-- Conversion

type NamingContext = [String]

-- Standard naming context provides default mappings between ASCII characters
-- and Indexes.
stdnc :: NamingContext
stdnc = chunksOf 1 ['a'..]

removeNames :: NamingContext -> Lambda -> ILambda
removeNames g (Var x) = IVar $ toIndex $ head (elemIndices x g)
removeNames g (Abs x t) = IAbs $ removeNames (x:g) t
removeNames g (App t1 t2) = IApp (removeNames g t1) (removeNames g t2)
