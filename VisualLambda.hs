module VisualLambda where

import Graphics.UI.Threepenny.Canvas
import qualified Graphics.UI.Threepenny            as UI
import           Graphics.UI.Threepenny.Core
import AST

data Direction = DirRight | DirDown

data VTerm = Wire [Point]
            | Clip [Point] Direction (Maybe VTerm)
            | Box Point (Maybe VTerm) (Maybe VTerm)

l2g :: Point -> LTerm -> VTerm
l2g p@(x,y) (LVar _) = Wire (p:(x+50,y):[])
l2g p@(x,y) (LAbs _ l) = Clip (p:(x+50,y):[]) DirRight (Just (l2g (x+50,y) l))
l2g p@(x,y) (LApp l m) = Box p (Just (l2g (x+20, y-30) l)) (Just (l2g (x+40, y) m))

drawWire :: Canvas -> [Point] -> UI ()
drawWire c (p:ps) = do
    beginPath c
    moveTo p c
    mapM_ (\x -> UI.lineTo x c) ps
    stroke c

drawClip :: Canvas -> Point -> Direction -> UI()
drawClip c p@(x,y) DirRight = do
    beginPath c
    moveTo (x+10, y-10) c
    lineTo p c
    lineTo (x+10, y+10) c
    stroke c
drawClip c p@(x,y) DirDown = do
    beginPath c
    moveTo (x+10, y-10) c
    lineTo p c
    lineTo (x-10, y-10) c
    stroke c

drawBox :: Canvas -> Point -> UI()
drawBox c p@(x,y) = do
    beginPath c
    moveTo (x,y-10) c
    lineTo (x,y+10) c
    lineTo (x+20,y+10) c
    lineTo (x+20,y-10) c
    closePath c
    stroke c

drawTerm :: Canvas -> VTerm -> UI()
drawTerm c (Wire ps) = do
    drawWire c ps
    drawClip c (last ps) DirRight
drawTerm c (Clip ps d l) = do
    drawWire c ps
    -- drawClip c (last ps) d
    mapM_ (\v -> drawTerm c v) l
drawTerm c (Box p@(x,y) l m) = do
    drawWire c [(x,y),(x+10, y)]
    drawBox c (x+10,y)
    drawWire c  [(x+20,y),(x+20, y-30)]
    drawWire c  [(x+20,y),(x+40, y)]
    mapM_ (\v -> drawTerm c v) l
    mapM_ (\v -> drawTerm c v) m
