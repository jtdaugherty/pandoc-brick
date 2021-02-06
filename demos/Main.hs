{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Brick
import Brick.Widgets.Pandoc
import Control.Monad (void)
import qualified Data.Text.IO as T
import qualified Graphics.Vty as V
import System.Exit (exitFailure)
import System.Environment (getArgs, getProgName)

import Commonmark (commonmark)
import Commonmark.Pandoc (Cm(unCm))
import Text.Pandoc.Builder (Blocks)

showHelp :: IO ()
showHelp = do
    pn <- getProgName
    putStrLn $ "Usage: " ++ pn ++ " <input.md>"

theme :: AttrMap
theme =
    attrMap V.defAttr
    [ (pandocEmphAttr,       fg V.white)
    , (pandocUnderlineAttr,  V.defAttr `V.withStyle` V.underline)
    , (pandocStrongAttr,     V.defAttr `V.withStyle` V.bold)
    , (pandocStrikeoutAttr,  V.defAttr `V.withStyle` V.strikethrough)
    , (pandocInlineCodeAttr, fg V.magenta)
    , (pandocCodeBlockAttr,  fg V.magenta)
    , (pandocHeaderAttr,     fg V.white `V.withStyle` V.bold `V.withStyle` V.underline)
    ]

draw :: Blocks -> [Widget ()]
draw bs =
    [viewport () Vertical $ cached () $ renderPandoc bs]

app :: App Blocks e ()
app =
    App { appDraw = draw
        , appChooseCursor = showFirstCursor
        , appHandleEvent = resizeOrQuit
        , appStartEvent = return
        , appAttrMap = const theme
        }

main :: IO ()
main = do
    args <- getArgs

    path <- case args of
        [p] -> return p
        _ -> showHelp >> exitFailure

    contents <- T.readFile path

    case commonmark path contents of
        Left e -> do
            print e
            exitFailure
        Right (cm::Cm () Blocks) ->
            void $ defaultMain app (unCm cm)
