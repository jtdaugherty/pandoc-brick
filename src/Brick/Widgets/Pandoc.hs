{-# LANGUAGE OverloadedStrings #-}
module Brick.Widgets.Pandoc
  ( renderPandoc

  -- * Attributes
  , pandocAttr
  , pandocEmphAttr
  , pandocUnderlineAttr
  , pandocStrongAttr
  , pandocStrikeoutAttr
  , pandocInlineCodeAttr
  , pandocCodeBlockAttr
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

pandocInlineCodeAttr :: AttrName
pandocInlineCodeAttr = pandocAttr <> "inlineCode"

pandocCodeBlockAttr :: AttrName
pandocCodeBlockAttr = pandocAttr <> "codeBlock"

-------------------------------------------------

renderPandoc :: P.Blocks -> Widget n
renderPandoc =
    withDefAttr pandocAttr .
    vBox . F.toList . fmap renderBlock . P.unMany

renderBlock :: P.Block -> Widget n
renderBlock (P.Plain is) =
    renderInlines is
renderBlock (P.Para is) =
    renderInlines is
renderBlock (P.LineBlock iss) =
    txt "TODO: line block"
renderBlock (P.CodeBlock _attr body) =
    withDefAttr pandocCodeBlockAttr $
    txt body
renderBlock (P.RawBlock fmt body) =
    txt "TODO: raw block"
renderBlock (P.BlockQuote bs) =
    txt "TODO: block quote"
renderBlock (P.OrderedList attrs bss) =
    txt "TODO: ordered list"
renderBlock (P.BulletList bss) =
    txt "TODO: bullet list"
renderBlock (P.DefinitionList [(is, bss)]) =
    txt "TODO: def list"
renderBlock (P.Header _lvl _attr is) =
    renderLine is
renderBlock (P.Table attr caption colSpecs head bodyList foot) =
    txt "TODO: tables"
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
renderInline (P.Superscript is) =
    txt "TODO: superscript"
renderInline (P.Subscript is) =
    txt "TODO: subscript"
renderInline (P.SmallCaps is) =
    txt "TODO: small caps"
renderInline (P.Quoted _quotTy is) =
    txt "TODO: quoted"
renderInline (P.Cite citations  is) =
    txt "TODO: cite"
renderInline (P.Code _attr t) =
    withDefAttr pandocInlineCodeAttr $
    txt t
renderInline (P.Math mathTy text) =
    txt "TODO: math"
renderInline (P.RawInline fmt text) =
    txt "TODO: raw inline"
renderInline (P.Link _attr is target) =
    txt "TODO: link"
renderInline (P.Image _attr is target) =
    txt "TODO: image"
renderInline (P.Note bs) =
    txt "TODO: note"
renderInline (P.Span _attr is) =
    renderInlines is

isLineBreak :: P.Inline -> Bool
isLineBreak P.SoftBreak = True
isLineBreak P.LineBreak = True
isLineBreak _ = False
