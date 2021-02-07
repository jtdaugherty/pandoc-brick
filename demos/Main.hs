{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Brick
import Brick.Widgets.Pandoc
import Brick.Widgets.Skylighting (attrMappingsForStyle)
import Control.Monad (void, when)
import Control.Monad.Trans (liftIO)
import Skylighting.Styles (pygments)
import Skylighting.Loader (loadSyntaxesFromDir)
import qualified Data.Text.IO as T
import qualified Graphics.Vty as V
import System.Exit (exitFailure)
import System.Environment (getArgs, getProgName)
import System.Console.GetOpt

import Commonmark (commonmark)
import Commonmark.Pandoc (Cm(unCm))
import Text.Pandoc.Builder (Blocks)

type St = (PandocRenderConfig, Blocks)

data Arg =
    Help
    | RespectSoftBreaks Bool
    | WrapLines Bool
    deriving (Eq, Show)

options :: [OptDescr Arg]
options =
    [ Option "h" ["help"] (NoArg Help)
      "Show this help"
    , Option "s" ["soft-breaks"] (NoArg $ RespectSoftBreaks True)
      "Respect soft line breaks"
    , Option "S" ["ignore-soft-breaks"] (NoArg $ RespectSoftBreaks False)
      "Ignore soft line breaks"
    , Option "w" ["wrap-long-lines"] (NoArg $ WrapLines True)
      "Respect soft line breaks"
    , Option "W" ["preserve-long-lines"] (NoArg $ WrapLines False)
      "Ignore soft line breaks"
    ]

showHelp :: IO ()
showHelp = do
    pn <- getProgName
    putStrLn $ usageInfo ("Usage: " ++ pn ++ " <input.md> [syntax XML directory]") options

theme :: AttrMap
theme =
    attrMap V.defAttr $
    [ (pandocEmphAttr,          fg V.white)
    , (pandocUnderlineAttr,     V.defAttr `V.withStyle` V.underline)
    , (pandocStrongAttr,        V.defAttr `V.withStyle` V.bold)
    , (pandocStrikeoutAttr,     V.defAttr `V.withStyle` V.strikethrough)
    , (pandocInlineCodeAttr,    fg V.magenta)
    , (pandocRawCodeBlockAttr,  fg V.magenta)
    , (pandocHeaderAttr,        fg V.white `V.withStyle` V.bold `V.withStyle` V.underline)
    , (pandocLinkAttr,          fg V.yellow)
    , (pandocBlockQuoteAttr,    fg V.cyan)
    ] <> attrMappingsForStyle pygments

draw :: St -> [Widget ()]
draw (renderConfig, bs) =
    [viewport () Vertical $ cached () $ renderPandoc renderConfig bs]

handleEvent :: s -> BrickEvent () e -> EventM () (Next s)
handleEvent s (VtyEvent (V.EvResize {})) = do
    invalidateCache
    continue s
handleEvent s (VtyEvent e) = do
    let vp = viewportScroll ()
    vty <- getVtyHandle
    (_, pageSize) <- liftIO $ V.displayBounds $ V.outputIface vty
    case e of
        V.EvKey V.KEsc []        -> halt s
        V.EvKey (V.KChar 'q') [] -> halt s
        V.EvKey V.KUp []         -> vScrollBy vp (-1) >> continue s
        V.EvKey (V.KChar 'k') [] -> vScrollBy vp (-1) >> continue s
        V.EvKey V.KDown []       -> vScrollBy vp 1 >> continue s
        V.EvKey (V.KChar 'j') [] -> vScrollBy vp 1 >> continue s
        V.EvKey V.KPageUp []     -> vScrollBy vp (-1 * pageSize) >> continue s
        V.EvKey V.KPageDown []   -> vScrollBy vp pageSize >> continue s
        V.EvKey V.KHome []       -> vScrollToBeginning vp >> continue s
        V.EvKey V.KEnd []        -> vScrollToEnd vp >> continue s
        _ -> continue s
handleEvent s _ = continue s

app :: App St e ()
app =
    App { appDraw = draw
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleEvent
        , appStartEvent = return
        , appAttrMap = const theme
        }

updateConfigFromArg :: Arg -> PandocRenderConfig -> PandocRenderConfig
updateConfigFromArg Help c =
    c
updateConfigFromArg (RespectSoftBreaks val) c =
    c { respectSoftLineBreaks = val }
updateConfigFromArg (WrapLines val) c =
    c { wrapLongLines = val }

main :: IO ()
main = do
    args <- getArgs
    let (parsedArgs, positional, errs) = getOpt Permute options args

    when (not $ null errs) $ do
        mapM_ putStrLn errs
        exitFailure

    when (Help `elem` parsedArgs) $ do
        showHelp
        exitFailure

    (mdPath, mSyntaxPath) <- case positional of
        [p] -> return (p, Nothing)
        [p, s] -> return (p, Just s)
        _ -> showHelp >> exitFailure

    contents <- T.readFile mdPath

    sMap <- case mSyntaxPath of
        Nothing -> return Nothing
        Just path -> do
            result <- loadSyntaxesFromDir path
            case result of
                Left _ -> return Nothing
                Right m -> return $ Just m

    let defaultRenderConfig =
            PandocRenderConfig { respectSoftLineBreaks = True
                               , wrapLongLines = False
                               , codeSyntaxMap = sMap
                               }
        renderConfig = foldr updateConfigFromArg defaultRenderConfig parsedArgs

    blocks <- case commonmark mdPath contents of
        Left e -> do
            print e
            exitFailure
        Right (cm::Cm () Blocks) ->
            return $ unCm cm

    void $ defaultMain app (renderConfig, blocks)
