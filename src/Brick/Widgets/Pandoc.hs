{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.Text as T
import qualified Data.Foldable as F
import qualified Text.Pandoc.Builder as P

data PandocRenderConfig =
    PandocRenderConfig { respectSoftLineBreaks :: Bool
                       }
    deriving (Show, Read, Eq)

defaultPandocRenderConfig :: PandocRenderConfig
defaultPandocRenderConfig =
    PandocRenderConfig { respectSoftLineBreaks = False
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
renderBlocks [b] = renderBlock b
renderBlocks (b:bs) = do
    b' <- renderBlock b
    bs' <- mapM renderBlock bs
    return $ vBox $ b' : (padTop (Pad 1) <$> bs')

renderBlock :: P.Block -> M (Widget n)
renderBlock (P.Plain is) =
    renderInlines is
renderBlock (P.Para is) =
    renderInlines is
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
renderBlock (P.Header _lvl _attr is) =
    withDefAttr pandocHeaderAttr <$> renderLine is
renderBlock (P.Table attr caption colSpecs head bodyList foot) =
    return $ txt "TODO: tables"
renderBlock (P.Div _attr bs) =
    vBox <$> mapM renderBlock bs
renderBlock P.HorizontalRule =
    return hBorder
renderBlock P.Null =
    return emptyWidget

renderInlines :: [P.Inline] -> M (Widget n)
renderInlines is = do
    theLines <- processLineBreaks is
    vBox <$> mapM renderLine theLines

renderLine :: [P.Inline] -> M (Widget n)
renderLine is = hBox <$> mapM renderInline is

processLineBreaks :: [P.Inline] -> M [[P.Inline]]
processLineBreaks [] = return []
processLineBreaks is = do
    soft <- asks respectSoftLineBreaks
    let (a, b) = span (not . isLineBreak) is
        isLineBreak P.LineBreak = True
        isLineBreak P.SoftBreak | soft = True
        isLineBreak _ = False
    rest <- processLineBreaks (dropWhile isLineBreak b)
    return $ a : rest

renderInline :: P.Inline -> M (Widget n)
renderInline (P.Str t) =
    return $ txt t
renderInline P.Space =
    return $ txt " "
renderInline P.SoftBreak =
    return $ txt " "
renderInline P.LineBreak =
    return emptyWidget
renderInline (P.Emph is) =
    withDefAttr pandocEmphAttr <$> renderInlines is
renderInline (P.Underline is) =
    withDefAttr pandocUnderlineAttr <$> renderInlines is
renderInline (P.Strong is) =
    withDefAttr pandocStrongAttr <$> renderInlines is
renderInline (P.Strikeout is) =
    withDefAttr pandocStrikeoutAttr <$> renderInlines is
renderInline (P.Superscript is) =
    return $ txt "TODO: superscript"
renderInline (P.Subscript is) =
    return $ txt "TODO: subscript"
renderInline (P.SmallCaps is) =
    return $ txt "TODO: small caps"
renderInline (P.Quoted _quotTy is) =
    return $ txt "TODO: quoted"
renderInline (P.Cite citations  is) =
    return $ txt "TODO: cite"
renderInline (P.Code _attr t) =
    return $ withDefAttr pandocInlineCodeAttr $ txt t
renderInline (P.Math mathTy text) =
    return $ txt "TODO: math"
renderInline (P.RawInline fmt text) =
    return $ txt "TODO: raw inline"
renderInline (P.Link _attr is target) =
    return $ txt "TODO: link"
renderInline (P.Image _attr is target) =
    return $ txt "TODO: image"
renderInline (P.Note bs) =
    return $ txt "TODO: note"
renderInline (P.Span _attr is) =
    renderInlines is
