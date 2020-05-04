{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

module TestingSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified ParserMonad as PM

spec :: Spec
spec = do
    describe "testing" $ do
        prop "testing" $ \x y ->
            add x y == add 0 1

add :: Int -> Int -> Int
add x y = x + y
