{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Brick.Widgets.Pandoc
  ( renderPandoc
  , PandocRenderConfig(..)

  -- * Attributes
  , pandocAttr
  , pandocEmphAttr
  , pandocUnderlineAttr
  , pandocStrongAttr
  , pandocStrikeoutAttr
  , pandocInlineCodeAttr
  , pandocCodeBlockAttr
  , pandocHeaderAttr
  )
where

import Brick
import Brick.Widgets.Border
import Control.Monad.Reader
import Data.List (splitAt)
import qualified Data.Text as T
import qualified Data.Foldable as F
import qualified Text.Pandoc.Builder as P

data PandocRenderConfig =
    PandocRenderConfig { respectSoftLineBreaks :: Bool
                       -- ^ Whether soft line breaks will be honored. If
                       -- 'True', soft line breaks will behave like hard
                       -- line breaks. If 'False', soft line breaks will
                       -- be converted to spaces.
                       , wrapLongLines :: Bool
                       -- ^ Whether to wrap long lines and headers.
                       }
    deriving (Show, Read, Eq)

defaultPandocRenderConfig :: PandocRenderConfig
defaultPandocRenderConfig =
    PandocRenderConfig { respectSoftLineBreaks = False
                       , wrapLongLines = False
                       }

-------------------------------------------------
-- Attributes

pandocAttr :: AttrName
pandocAttr = "pandoc"

pandocEmphAttr :: AttrName
pandocEmphAttr = pandocAttr <> "emph"

pandocUnderlineAttr :: AttrName
pandocUnderlineAttr = pandocAttr <> "underline"

pandocStrongAttr :: AttrName
pandocStrongAttr = pandocAttr <> "strong"

pandocStrikeoutAttr :: AttrName
pandocStrikeoutAttr = pandocAttr <> "strikeout"

pandocInlineCodeAttr :: AttrName
pandocInlineCodeAttr = pandocAttr <> "inlineCode"

pandocCodeBlockAttr :: AttrName
pandocCodeBlockAttr = pandocAttr <> "codeBlock"

pandocHeaderAttr :: AttrName
pandocHeaderAttr = pandocAttr <> "header"

-------------------------------------------------

type M a = Reader PandocRenderConfig a

renderPandoc :: PandocRenderConfig -> P.Blocks -> Widget n
renderPandoc cfg bs =
    withDefAttr pandocAttr result
    where
        result = runReader (renderBlocks $ F.toList $ P.unMany bs) cfg

renderBlocks :: [P.Block] -> M (Widget n)
renderBlocks [] = return emptyWidget
renderBlocks (b:bs) = do
    b' <- renderBlock b
    bs' <- mapM renderBlock bs
    return $ vBox $ b' : (padTop (Pad 1) <$> bs')

renderInlinesM :: [P.Inline] -> M (Widget n)
renderInlinesM is =
    renderInlines <$> (asks respectSoftLineBreaks)
                  <*> (asks wrapLongLines)
                  <*> (pure is)

renderBlock :: P.Block -> M (Widget n)
renderBlock (P.Plain is) = do
    renderInlinesM is
renderBlock (P.Para is) = do
    renderInlinesM is
renderBlock (P.LineBlock iss) =
    return $ txt "TODO: line block"
renderBlock (P.CodeBlock _attr body) =
    return $ withDefAttr pandocCodeBlockAttr $ txt body
renderBlock (P.RawBlock fmt body) =
    return $ txt "TODO: raw block"
renderBlock (P.BlockQuote bs) =
    return $ txt "TODO: block quote"
renderBlock (P.OrderedList attrs bss) =
    return $ txt "TODO: ordered list"
renderBlock (P.BulletList bss) =
    return $ txt "TODO: bullet list"
renderBlock (P.DefinitionList [(is, bss)]) =
    return $ txt "TODO: def list"
renderBlock (P.Header _lvl _attr is) = do
    wrap <- asks wrapLongLines
    return $ withDefAttr pandocHeaderAttr $ renderLine wrap is
renderBlock (P.Table attr caption colSpecs head bodyList foot) =
    return $ txt "TODO: tables"
renderBlock (P.Div _attr bs) =
    vBox <$> mapM renderBlock bs
renderBlock P.HorizontalRule =
    return hBorder
renderBlock P.Null =
    return emptyWidget

renderInlines :: Bool -> Bool -> [P.Inline] -> Widget n
renderInlines soft wrap is =
    let theLines = breakLines $ if soft
                                then is
                                else convertSoftLineBreak <$> is
    in vBox $ renderLine wrap <$> theLines

convertSoftLineBreak :: P.Inline -> P.Inline
convertSoftLineBreak P.SoftBreak = P.Space
convertSoftLineBreak i = i

renderLine :: Bool -> [P.Inline] -> Widget n
renderLine wrap is =
    Widget Fixed Fixed $ do
        ctx <- getContext
        let w = availWidth ctx
            maybeWrap = if wrap
                        then wrapInlines w
                        else (:[])
        render $ vBox $ (hBox . fmap renderInline) <$> maybeWrap is

breakLines :: [P.Inline] -> [[P.Inline]]
breakLines [] = []
breakLines is =
    let (a, b) = span (not . isLineBreak) is
        rest = breakLines (dropWhile isLineBreak b)
    in a : rest

renderInline :: P.Inline -> Widget n
renderInline (P.Str t) =
    txt t
renderInline P.Space =
    txt " "
renderInline P.SoftBreak =
    emptyWidget
renderInline P.LineBreak =
    emptyWidget
renderInline (P.Emph is) =
    withDefAttr pandocEmphAttr $ renderInlines False False is
renderInline (P.Underline is) =
    withDefAttr pandocUnderlineAttr $ renderInlines False False is
renderInline (P.Strong is) =
    withDefAttr pandocStrongAttr $ renderInlines False False is
renderInline (P.Strikeout is) =
    withDefAttr pandocStrikeoutAttr $ renderInlines False False is
renderInline (P.Superscript is) =
    renderInlines False False $ P.Str "(" : (is <> [P.Str ")"])
renderInline (P.Subscript is) =
    renderInlines False False $ P.Str "(" : (is <> [P.Str ")"])
renderInline (P.SmallCaps is) =
    renderInlines False False $ capitalizeInline <$> is
renderInline (P.Quoted _quotTy is) =
    txt "TODO: quoted"
renderInline (P.Cite citations  is) =
    txt "TODO: cite"
renderInline (P.Code _attr t) =
    withDefAttr pandocInlineCodeAttr $ txt t
renderInline (P.Math mathTy text) =
    txt "TODO: math"
renderInline (P.RawInline (P.Format fmt) t) =
    withDefAttr pandocInlineCodeAttr $
    txt $ "[" <> fmt <> "] " <> t
renderInline (P.Link _attr is target) =
    txt "TODO: link"
renderInline (P.Image _attr is target) =
    txt "TODO: image"
renderInline (P.Note bs) =
    txt "TODO: note"
renderInline (P.Span _attr is) =
    renderInlines False False is

capitalizeInline :: P.Inline -> P.Inline
capitalizeInline (P.Str t) =
    P.Str $ T.toUpper t
capitalizeInline (P.RawInline fmt t) =
    P.RawInline fmt $ T.toUpper t
capitalizeInline (P.Emph is) =
    P.Emph $ capitalizeInline <$> is
capitalizeInline (P.Underline is) =
    P.Underline $ capitalizeInline <$> is
capitalizeInline (P.Strong is) =
    P.Strong $ capitalizeInline <$> is
capitalizeInline (P.Strikeout is) =
    P.Strikeout $ capitalizeInline <$> is
capitalizeInline (P.Superscript is) =
    P.Superscript $ capitalizeInline <$> is
capitalizeInline (P.Subscript is) =
    P.Subscript $ capitalizeInline <$> is
capitalizeInline (P.SmallCaps is) =
    P.SmallCaps $ capitalizeInline <$> is
capitalizeInline (P.Quoted quotTy is) =
    P.Quoted quotTy $ capitalizeInline <$> is
capitalizeInline (P.Link attr is (url, title)) =
    P.Link attr (capitalizeInline <$> is) (url, T.toUpper title)
capitalizeInline (P.Image attr is (url, title)) =
    P.Image attr (capitalizeInline <$> is) (url, T.toUpper title)
capitalizeInline (P.Span attr is) =
    P.Span attr $ capitalizeInline <$> is
capitalizeInline i = i

wrapInlines :: Int -> [P.Inline] -> [[P.Inline]]
wrapInlines _ [] = []
wrapInlines width (i:rest) | inlineWidth i > width =
    -- If the line starts with a token that is larger than the allowed
    -- width, just leave it on its own line.
    [i] : wrapInlines width rest
wrapInlines width inlines =
    let go _ [] = 0
        go w (i:is) =
            let iWidth = inlineWidth i
            in if | iWidth < w  -> 1 + go (w - iWidth) is
                  | iWidth == w -> 1
                  | otherwise   -> 0
        curLineCount = go width inlines
        (curLine, rest) = splitAt curLineCount inlines
    in curLine : wrapInlines width (dropWhile isSpace rest)

isSpace :: P.Inline -> Bool
isSpace P.Space = True
isSpace _ = False

isLineBreak :: P.Inline -> Bool
isLineBreak P.LineBreak = True
isLineBreak P.SoftBreak = True
isLineBreak _ = False

inlineWidth :: P.Inline -> Int
inlineWidth P.Space                      = 1
inlineWidth P.SoftBreak                  = 0
inlineWidth P.LineBreak                  = 0
inlineWidth (P.Code _ t)                 = textWidth t
inlineWidth (P.Math _ t)                 = textWidth t
inlineWidth (P.RawInline (P.Format f) t) = textWidth f + 3 + textWidth t
inlineWidth (P.Str t)                    = textWidth t
inlineWidth (P.Link _ _ (url, ""))       = textWidth url
inlineWidth (P.Image _ _ (url, ""))      = textWidth url
inlineWidth (P.Link _ _ (url, title))    = textWidth title
inlineWidth (P.Image _ _ (url, title))   = textWidth title
inlineWidth (P.Emph is)                  = sum $ inlineWidth <$> is
inlineWidth (P.Underline is)             = sum $ inlineWidth <$> is
inlineWidth (P.Strong is)                = sum $ inlineWidth <$> is
inlineWidth (P.Strikeout is)             = sum $ inlineWidth <$> is
inlineWidth (P.Superscript is)           = 2 + (sum $ inlineWidth <$> is)
inlineWidth (P.Subscript is)             = 2 + (sum $ inlineWidth <$> is)
inlineWidth (P.SmallCaps is)             = sum $ inlineWidth <$> is
inlineWidth (P.Quoted _ is)              = sum $ inlineWidth <$> is
inlineWidth (P.Span _ is)                = sum $ inlineWidth <$> is
-- I'm not sure how these should be visually represented, so for now I'm
-- not going to bother to compute the width.
inlineWidth (P.Cite _ _)                 = 0
-- It's weird to compute the width of a block sequence, so I am not
-- going to bother. I am not yet sure how/when this kind of node would
-- even get created.
inlineWidth (P.Note _)                   = 0
