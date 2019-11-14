module ChurchEncodings where

import           AST
import           Data.Either
import           Parser

-- A collection of common church encodings for testing.
-- functions all begin with the prefix c.

cid = "\\x.x"
ctrue = "\\tf.t"
cfalse = "\\tf.f"
cif = "\\btf.btf"

-- Term Constructor
tc :: String -> LTerm
tc s = fromRight (LVar "parseError") (parseTerm s)
