{-# LANGUAGE OverloadedStrings #-}
module Brick.Widgets.Pandoc
  ( renderPandoc

  -- * Attributes
  , pandocAttr
  , pandocEmphAttr
  , pandocUnderlineAttr
  , pandocStrongAttr
  , pandocStrikeoutAttr
  )
where

import Brick
import Brick.Widgets.Border
import qualified Data.Text as T
import qualified Data.Foldable as F
import qualified Text.Pandoc.Builder as P

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

-------------------------------------------------

renderPandoc :: P.Blocks -> Widget n
renderPandoc = vBox . F.toList . fmap renderBlock . P.unMany

renderBlock :: P.Block -> Widget n
renderBlock (P.Plain is) =
    renderInlines is
renderBlock (P.Para is) =
    renderInlines is
-- renderBlock (P.LineBlock iss) =
--     emptyWidget
-- renderBlock (P.CodeBlock attr body) =
--     emptyWidget
-- renderBlock (P.RawBlock fmt body) =
--     emptyWidget
-- renderBlock (P.BlockQuote bs) =
--     emptyWidget
-- renderBlock (P.OrderedList attrs bss) =
--     emptyWidget
-- renderBlock (P.BulletList bss) =
--     emptyWidget
-- renderBlock (P.DefinitionList [(is, bss)]) =
--     emptyWidget
renderBlock (P.Header _lvl _attr is) =
    renderLine is
-- renderBlock (P.Table attr caption colSpecs head bodyList foot) =
--     emptyWidget
renderBlock (P.Div _attr bs) =
    vBox $ renderBlock <$> bs
renderBlock P.HorizontalRule =
    hBorder
renderBlock P.Null =
    emptyWidget

renderInlines :: [P.Inline] -> Widget n
renderInlines = vBox . fmap renderLine . processLineBreaks

renderLine :: [P.Inline] -> Widget n
renderLine = hBox . fmap renderInline

processLineBreaks :: [P.Inline] -> [[P.Inline]]
processLineBreaks [] = []
processLineBreaks is =
    let (a, b) = span (not . isLineBreak) is
    in a : processLineBreaks (dropWhile isLineBreak b)

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
    withDefAttr pandocEmphAttr $
    renderInlines is
renderInline (P.Underline is) =
    withDefAttr pandocUnderlineAttr $
    renderInlines is
renderInline (P.Strong is) =
    withDefAttr pandocStrongAttr $
    renderInlines is
renderInline (P.Strikeout is) =
    withDefAttr pandocStrikeoutAttr $
    renderInlines is
-- renderInline (P.Superscript is) =
-- renderInline (P.Subscript is) =
-- renderInline (P.SmallCaps is) =
-- renderInline (P.Quoted QuoteType is) =
-- renderInline (P.Cite [Citation]  is) =
-- renderInline (P.Code Attr Text) =
-- renderInline (P.Math MathType Text) =
-- renderInline (P.RawInline Format Text) =
-- renderInline (P.Link Attr is Target) =
-- renderInline (P.Image Attr is Target) =
-- renderInline (P.Note bs) =
renderInline (P.Span _attr is) =
    renderInlines is

isLineBreak :: P.Inline -> Bool
isLineBreak P.SoftBreak = True
isLineBreak P.LineBreak = True
isLineBreak _ = False
