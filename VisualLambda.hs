{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module VisualLambda where

import qualified Lambda as L
import Diagrams.Prelude
import Diagrams.Backend.Rasterific

-- This code is a re-implementation of Paul Brauners's implementation
-- of John Tromp's lambda diagrams: https://github.com/polux/lambda-diagrams
-- Particuarly Main.hs lines 316 to 346
-- This code has also been modified and extended to work with my
-- implementation of lambda terms and to display free variables
-- modification is permitted under the Apache 2.0 license
-- http://www.apache.org/licenses/LICENSE-2.0

type Width = Int
type Height = Int

draw :: L.Lambda -> Diagram B
draw t = fig # lwL 0.5 # frame 1
    where binders = L.scopeLevel t
          (fig, _, _) = figure binders t

figure :: L.ScopeLevel -> L.Lambda -> (Diagram B, Width, Height)
figure s (L.Var x) = (fig, 1, 0)
    where minreach = min s (L.toInt x)
          reach = 1 + fromIntegral minreach
          fig = (phantom (hrule 2 :: Diagram B)) <> vrule reach # alignB
figure s (L.Abs t) = ((bar <> fig # translateY (-1)), w, h+1)
    where (fig, w, h) = figure s t
          bar = hrule (fromIntegral (2 * w) - 0.5) # alignL # translateX (-0.75)
figure s (L.App t1 t2) =
    (((left <> taill) ||| (right <> tailr)) <> bar, wl + wr, hl + delta1 + 1)
   where
    (left, wl, hl) = figure s t1
    (right, wr, hr) = figure s t2
    delta1         = max 0 (hr - hl)
    delta2         = max 0 (hl - hr)
    taill          = vrule (fromIntegral $ delta1 + 1) # alignT # translateY (fromIntegral (-hl))
    tailr          = vrule (fromIntegral delta2) # alignT # translateY (fromIntegral (-hr))
    bar =
      hrule (fromIntegral $ (2 * wl)) # alignL # translateY (fromIntegral (-hl - delta1)) # lineCap LineCapSquare

render :: L.Lambda -> FilePath -> IO ()
render t f = renderRasterific f (dims2D 1600 900) (draw t)

-- test :: L.Lambda
-- test = removeNames stdnc $ fst $ head $ parseTerm "($eq 1) 1"

-- main = renderRasterific "test.png" (dims2D 1600 900) (draw test)
