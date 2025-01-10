{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Emitter where

import Data.Text (Text)
import qualified Data.Text as T
import Types ( Element(..) )

data MathExpr = InlineExpr Text | BlockExpr Text

boldGenerator :: [Element] -> Text
boldGenerator text = "<b>" <> T.concat (map emitHtml text) <> "</b>"

italicGenerator :: [Element] -> Text
italicGenerator text = "<i>" <> T.concat (map emitHtml text) <> "</i>"

linkGenerator :: [Element] -> Text -> Text
linkGenerator text url = "<a href=\"" <> url <> "\">" <>
                         T.concat (map emitHtml text) <> "</a>"

picGenerator :: Text -> Text -> Text
picGenerator alt url = "<img class=\"post-img\" " <>
                       "alt=\"" <> alt <> "\" " <>
                       "src=\"" <> url <> "\" " <> "width=\"800\" height=\"600\">"

headGenerator :: [Element] -> Text
headGenerator text = "<h2 class=\"post-subtitle\">" <>
                     T.concat (map emitHtml text) <> "</h2>\n" <>
                     "<div class=\"sp\"></div>"

icodeGenerator :: Text -> Text
icodeGenerator text = "<code class=\"inline-code\">" <> text <> "</code>"

cblockGenerator :: Text -> Text -> Text
cblockGenerator lang content = "<pre>\n<code class=\"language-" <>
                               lang <> "\">\n" <>
                               content <> "</code></pre>"

citGenerator :: [Element] -> Text
citGenerator content = "<blockquote>\n<div class=\"cursor\">></div>\n" <>
                       T.concat (map emitHtml content) <> "\n</blockquote>"


refLinkGenerator :: Char -> Text
refLinkGenerator num = "<a id=\"ref-" <> T.singleton num <> "\" href=\"#foot-" <>
                       T.singleton num <> "\">[" <>
                       T.singleton num <> "]</a>"

refGenerator :: Char -> [Element] -> Text
refGenerator num text = "<p id=\"foot-" <> T.singleton num <> "\">" <>
                        "[" <> T.singleton num <> "]: " <>
                        T.concat (map emitHtml text) <> " " <>
                        "<a href=\"#ref-" <> T.singleton num <>
                        "\">&#8617;</a></p>"

mathExprGenerator :: MathExpr -> Text
mathExprGenerator (InlineExpr expr)  = "\\(" <> expr <> "\\)"
mathExprGenerator (BlockExpr expr) = "$$" <> expr <> "$$"

emitHtml :: Element -> Text
emitHtml (Bold text) = boldGenerator text
emitHtml (Italic text) = italicGenerator text
emitHtml (Link text url) = linkGenerator text url
emitHtml (Picture alt url) = picGenerator alt url
emitHtml (Header text) = headGenerator text
emitHtml (ICode text) = icodeGenerator text
emitHtml (CBlock lang content) = cblockGenerator lang content
emitHtml (Citation cit) = citGenerator cit
emitHtml (RefLink num) = refLinkGenerator num
emitHtml (Ref num ref) = refGenerator num ref
emitHtml (IMathExpr expr) = mathExprGenerator (InlineExpr expr)
emitHtml (MathExpr expr) = mathExprGenerator (BlockExpr expr)
emitHtml (Text text) = text
