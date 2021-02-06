module Main where

import Brick
import Brick.Widgets.Pandoc

main :: IO ()
main = simpleMain (renderPandoc mempty :: Widget ())
