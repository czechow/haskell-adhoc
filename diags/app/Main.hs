{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where


import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
--import Graphics.SVGFonts

myCircle :: Diagram B
myCircle = text "Wania" <> circle 1 <> square (sqrt 2) # showOrigin

-- text' d s = (strokeP $ textSVG' (TextOpts lin2 INSIDE_H KERN False d d) s)
--            # lw none

--example = text' 5 "Hello" # fc blue ||| text' 3 "world" # fc green

main = mainWith myCircle
