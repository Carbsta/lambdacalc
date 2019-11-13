module Evaluator where

import AST
import Data.List
import Parser

-- A variable x is bound to an abstraction when it occurs in the
-- body t of an abstraction \x.t
-- x is free if it appears in a position where it is not bound
-- by an enclosing abstraction on x.
data BoundLTerm = BVal (Maybe Int, LVar)
                | BAbs (LVar, Int) BoundLTerm
                | BApp BoundLTerm BoundLTerm
                  deriving Show


bind :: LTerm -> Int -> Bindings -> BoundLTerm
bind (L v) i b = case i `lookup` b of
        Nothing -> BVal (Nothing, v)
        Just v' -> case v == v' of
                True -> BVal ()

-- eval :: Env -> LTerm -> Value
-- eval env term = case term of
--         L v      -> Var v
--         LAbs v a -> Closure a env
--         LApp a b ->
--             let Closure c env' = eval env a in
--             let v = eval env b in
--             eval (v : env') c

-- data Value = Var LVar
--            | Closure ScopedLTerm
--            deriving (Show, Eq)
--
-- scoper :: LTerm -> Int -> Env -> ScopedLTerm
-- scoper (L v) s e = case (Var v) `elemIndex` e of
--         Nothing -> ((SVal s), e++[Var v])
--         Just i  -> ((SVal i), e)
-- scoper (LAbs v l) s e = case (Var v) `elemIndex` e of
--         Nothing -> ((SAbs s l'), e'')
--         Just i  -> ((SAbs i l'), e')
--                    where (l',e') = scoper l



-- \t. \f. t;
tru :: LTerm
tru = LAbs 't' (LAbs 'f' (L 't'))

-- \t. \f. f;
fls :: LTerm
fls = LAbs 't' (LAbs 'f' (L 'f'))

-- \b. \t. \f. b t f
lif :: LTerm
lif = LAbs 'b' (LAbs 't' (LAbs 'f' (LApp (LApp (L 'b') (L 't')) (L 'f'))))

lid :: LTerm
lid = LAbs 'x' (L 'x')

test :: LTerm
test = LApp (lid) (LApp (lid) (LAbs 'z' (LApp (lid) (L 'z'))))

testif :: LTerm
testif = LApp (LApp (LApp lif tru) (L 'y')) (L 'x')
