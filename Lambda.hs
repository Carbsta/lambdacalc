{-|
de Bruijn notation of lambda terms, with incidices in a unary representation as
successors of zero. Zero corresponds to the closest binder to a variable,
with the successor S increasing the distance between variable and binder by one.
If a variable index is larger than the number of binders between the term and
root of the expression (e.g the index points to a binder outside of the term)
then that index represents a free variable within that term.
-}

module Lambda where

data Index = S Index | Zero

instance Show Index where
    showsPrec _ = shows . toInt

toIndex :: (Num a, Eq a) => a -> Index
toIndex 0 = Zero
toIndex n = S $ toIndex (n-1)

toInt :: (Num a) => Index -> a
toInt Zero = 0
toInt (S i) = 1 + toInt i

data Lambda = Var Index
            | Abs Lambda
            | App Lambda Lambda


instance Show Lambda where
    showsPrec _ (Var n) = shows n
    showsPrec _ (Abs lt) = (:) 'λ' . shows lt
    showsPrec _ (App lt rt) = (:) '(' . shows lt . (:) ')'
        .  (:) '(' . shows rt . (:) ')'

shift :: (Num a, Ord a)=> a -> Lambda -> a -> Lambda
shift d (Var k) c
    | toInt(k) < c = Var k
    | otherwise = Var $ toIndex $ toInt(k) + d
shift d (Abs l) c = Abs $ shift d l (c+1)
shift d (App l r) c = App (shift d l c) (shift d r c)

subst :: (Num a, Eq a, Ord a) => a -> Lambda -> Lambda -> Lambda
subst j s (Var k)
    | toInt(k) == j = s
    | otherwise = Var k
subst j s (Abs l) = Abs $ subst (j+1) (shift 1 s 0) l
subst j s (App l r) = App (subst j s l) (subst j s r)

-- Evaluation:
--   t1 -> t1' / t1 t2 -> t1' t2 (E-App1)
--   t2 -> t2' / v1 t2 -> v1 t2' (E-App2)
-- (\x.t12) v2 -> ^-1[0 => ^1(v2)]t12 (E-AppAbs) - beta reduction
eval :: Lambda -> Lambda
eval (App (Abs l) v@(Abs _)) = shift (-1) (subst 0 (shift 1 v 0) l) 0
eval (App v@(Abs _) t) = App v (eval t)
eval (App l r) = App (eval l) r
eval x = x
