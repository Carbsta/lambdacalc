module Main where

import           AST
import           VisualLambda
import           Evaluator
import           Parser
import           Text.Parsec
import qualified Graphics.UI.Threepenny            as UI
import           Graphics.UI.Threepenny.Core
import           Data.Maybe

main :: IO ()
main = do
    startGUI defaultConfig
        { jsPort       = Just 8023
        , jsStatic     = Just ""
        } setup

canWidth,canHeight :: Num a => a
canWidth  = 800
canHeight = 800

mkCanvas :: Int -> Int -> UI Element
mkCanvas h w = UI.canvas # set UI.height h
      # set UI.width w
      # set style [("border", "solid black 1px"), ("background", "#eee")]

mkToolbar :: UI Element
mkToolbar = UI.div #. "toolbar"

testPoints :: [UI.Point]
testPoints = [(50,50),(50,100),(100,100)]

setup :: Window -> UI ()
setup window = do
   stdlib <- liftIO $ fmap link $ parseFileName parseFile "StandardLibrary.lam"
   return window # set UI.title "Term Builder"
   toolbar <- mkToolbar
   canvas <- mkCanvas canWidth canHeight # set UI.droppable True
   --UI.fillRect (50.0,50.0) 50.0 50.0 canvas
   drawTerm canvas (l2g (50.0,400.0) (fromJust $ lookup "four" stdlib))
   UI.addStyleSheet window "style.css"
   UI.addStyleSheet window "widgets.css"
   -- b <- UI.div #. "drag-me" #  set text "Drag me around!" # set UI.draggable True # set UI.dragData "dragging"
   getBody window #+ [column [pure canvas]]
   return ()
