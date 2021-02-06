{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Brick
import Brick.Widgets.Pandoc
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
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

renderConfig :: PandocRenderConfig
renderConfig =
    PandocRenderConfig { respectSoftLineBreaks = True
                       }

draw :: Blocks -> [Widget ()]
draw bs =
    [viewport () Vertical $ cached () $ renderPandoc renderConfig bs]

handleEvent :: s -> BrickEvent () e -> EventM () (Next s)
handleEvent s (VtyEvent e) = do
    let vp = viewportScroll ()
    vty <- getVtyHandle
    (_, pageSize) <- liftIO $ V.displayBounds $ V.outputIface vty
    case e of
        V.EvKey V.KEsc []        -> halt s
        V.EvKey (V.KChar 'q') [] -> halt s
        V.EvKey V.KUp []         -> vScrollBy vp (-1) >> continue s
        V.EvKey V.KDown []       -> vScrollBy vp 1 >> continue s
        V.EvKey V.KPageUp []     -> vScrollBy vp (-1 * pageSize) >> continue s
        V.EvKey V.KPageDown []   -> vScrollBy vp pageSize >> continue s
        V.EvKey V.KHome []       -> vScrollToBeginning vp >> continue s
        V.EvKey V.KEnd []        -> vScrollToEnd vp >> continue s
        _ -> continue s
handleEvent s _ = continue s

app :: App Blocks e ()
app =
    App { appDraw = draw
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleEvent
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
