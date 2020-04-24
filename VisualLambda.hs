{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module VisualLambda where

import qualified Lambda as L
import Diagrams.Prelude
import Diagrams.Backend.Rasterific
import Parser
import Conversion

-- toDouble :: L.Index -> Double
-- toDouble = fromIntegral . L.toInt
--
-- draw :: L.Lambda -> Diagram B
-- draw t = fig where (fig, _, _) = figure t
--
-- -- helper function for drawing the diagram returns the figure and width and height
-- figure :: L.Lambda -> (Diagram B, Int, Int)
-- figure (L.Var x) = (fig, 1, 0)
--     where fig = (phantom (hrule 2 :: Diagram B)) <> vrule (1 + toDouble x) # alignB
-- figure (L.Abs t) = ((binder <> fig), w, h+1)
--     where (fig, w, h) = figure t
--           binder = hrule (fromIntegral (2 * w) - 0.5) # alignL #translateX (-0.75)
-- figure (L.App t1 t2) =
--     (((fig1 <> tail1) ||| (fig2 <> tail2)) <> bar, w1 + w2, h1 + delta1 + 1)
--    where
--     (fig1, w1, h1) = figure t1
--     (fig2, w2, h2) = figure t2
--     delta1         = max 0 (h2 - h1)
--     delta2         = max 0 (h1 - h2)
--     tail1          = vrule (fromIntegral $ delta1 + 1) # alignT # translateY (fromIntegral (-h1))
--     tail2          = vrule (fromIntegral delta2) # alignT # translateY (fromIntegral (-h2))
--     bar =
--       hrule (fromIntegral $ (2 * w1)) # alignL # translateY (fromIntegral (-h1 - delta1)) # lineCap LineCapSquare

double :: Int -> Double
double = fromIntegral

vbar :: Int -> Diagram B
vbar n = vrule (double n) # alignT

hbar :: Int -> Diagram B
hbar n = hrule (double n) # alignL

draw :: L.Lambda -> Diagram B
draw t = let (fig, _, _) = draw' t in fig # lwL 0.5 # frame 1
 where
  draw' :: L.Lambda -> (Diagram B, Int, Int)
  draw' (L.Abs t) = (binder <> (fig # translateY (-1)), h + 1, w)
   where
    (fig, h, w) = draw' t
    binder      = hrule (double (2 * w) - 0.5) # alignL # translateX (-0.75)
  draw' (L.Var i) = (fig, 0, 1)
   where
    fig = (phantom (hrule 2 :: Diagram B) <> vrule (double $ (L.toInt i) + 1)) # alignB
  draw' (L.App t1 t2) =
    (((fig1 <> tail1) ||| (fig2 <> tail2)) <> bar, h1 + delta1 + 1, w1 + w2)
   where
    (fig1, h1, w1) = draw' t1
    (fig2, h2, w2) = draw' t2
    delta1         = max 0 (h2 - h1)
    delta2         = max 0 (h1 - h2)
    tail1          = vbar (delta1 + 1) # translateY (double (-h1))
    tail2          = vbar delta2 # translateY (double (-h2))
    bar =
      hbar (2 * w1) # translateY (double (-h1 - delta1)) # lineCap LineCapSquare

render :: L.Lambda -> FilePath -> IO ()
render t f = renderRasterific f (dims2D 1600 900) (draw t)

-- test :: L.Lambda
-- test = removeNames stdnc $ fst $ head $ parseTerm "($eq 1) 1"

-- main = renderRasterific "test.png" (dims2D 1600 900) (draw test)
