module ChurchEncodings where

import           AST
import           Data.Either
import           Parser

-- A collection of common church encodings for testing.
-- functions all begin with the prefix c.

cid = "(\\x.x)"
ctrue = "(\\tf.t)"
cfalse = "(\\tf.f)"
cif = "(\\btf.btf)"
cpair = "(\\fsb.bfs)"
cfst = "(\\p.p"++ctrue++")"
csnd = "(\\p.p"++cfalse++")"
czero = "(\\sz.z)"
cone = "(\\sz.sz)"
ctwo = "(\\sz.s(sz))"
cthree = "(\\sz.s(s(sz)))" -- etc
cscc = "(\\nsz.s(nsz))"
cplus = "(\\mnsz.ms(nsz))"
ctimes = "(\\mn.m("++cplus++"n)"++czero++")"

omega = "(\\x.xx)(\\y.yy)"

cpairtest = cfst++"("++cpair++"vw)"
scctest = cscc++czero
addtest = cplus++czero++cone

fixpoint = "(\\f. ((\\x.(f (x x))) (\\x.(f (x x))))) (\\x.x)"


-- Term Constructor
tc :: String -> LTerm
tc s = fromRight (LVar "parseError") (parseTerm s)
