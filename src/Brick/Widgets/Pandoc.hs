{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Brick.Widgets.Pandoc
  ( renderPandoc
  )
where

import Brick
import Brick.Widgets.Border
import qualified Data.Text as T
import qualified Data.Foldable as F
import qualified Text.Pandoc.Builder as P

renderPandoc :: P.Blocks -> Widget n
renderPandoc = vBox . F.toList . fmap renderBlock . P.unMany

renderBlock :: P.Block -> Widget n
renderBlock (P.Plain is) =
    hBox $ renderInline <$> is
renderBlock (P.Para is) =
    hBox $ renderInline <$> is
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
-- renderBlock (P.Header lvl attr is) =
--     emptyWidget
-- renderBlock (P.Table attr caption colSpecs head bodyList foot) =
--     emptyWidget
-- renderBlock (P.Div attr bs) =
--     emptyWidget
renderBlock P.HorizontalRule =
    hBorder
renderBlock P.Null =
    emptyWidget

renderInline :: P.Inline -> Widget n
renderInline (P.Str t) =
    txt t
renderInline P.Space =
    txt " "
renderInline P.SoftBreak =
    emptyWidget -- TODO FIXME
renderInline P.LineBreak =
    emptyWidget -- TODO FIXME
-- renderInline (P.Emph is) =
-- renderInline (P.Underline is) =
-- renderInline (P.Strong is) =
-- renderInline (P.Strikeout is) =
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
-- renderInline (P.Span Attr is) =
