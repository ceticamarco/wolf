{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Emitter where

import Data.Text (Text)
import qualified Data.Text as T
import Types ( Element(..) )

data MathExpr = InlineExpr Text | BlockExpr Text

boldGenerator :: [Element] -> Text
boldGenerator text = "<b>" <> content <> "</b>" 
    where content = foldr ((<>) . emitHtml) T.empty text

italicGenerator :: [Element] -> Text
italicGenerator text = "<i>" <> content <> "</i>"
    where content = foldr ((<>) . emitHtml) T.empty text

linkGenerator :: [Element] -> Text -> Text
linkGenerator text url = "<a href=\"" <> url <> "\">" <> content <> "</a>"
    where content = foldr ((<>) . emitHtml) T.empty text

picGenerator :: Text -> Text -> Text
picGenerator alt url = "<img class=\"post-img\" " <>
                       "alt=\"" <> alt <> "\" " <>
                       "src=\"" <> url <> "\" " <> "width=\"800\" height=\"600\">"

headGenerator :: [Element] -> Text
headGenerator text = "<h2 id=\"" <> fmtID <> "\" " <>
                     "class=\"post-subtitle\">" <> content <> 
                     " <a class=\"head-tag\" href=\"#" <> fmtID <> "\">§</a></h2>\n" <>
                     "<div class=\"sp\"></div>"
    where
        fmtID = T.map (\c -> if c == ' ' then '_' else c) content
        content = foldr ((<>) . emitHtml) T.empty text

icodeGenerator :: Text -> Text
icodeGenerator text = "<code class=\"inline-code\">" <> text <> "</code>"

cblockGenerator :: Text -> Text -> Text
cblockGenerator lang content = "<pre>\n<code class=\"language-" <>
                               lang <> "\">\n" <>
                               content <> "</code></pre>"

citGenerator :: [Element] -> Text
citGenerator text = "<blockquote>\n<div class=\"cursor\">></div>\n" <> 
                        content <> "\n</blockquote>"
    where content = foldr ((<>) . emitHtml) T.empty text


refLinkGenerator :: Char -> Text
refLinkGenerator num = "<a id=\"ref-" <> ref <> "\" href=\"#foot-" <> ref <> "\">[" <>
                       ref <> "]</a>"
    where ref = T.singleton  num

refGenerator :: Char -> [Element] -> Text
refGenerator num text = "<p id=\"foot-" <> ref <> "\">" <>
                        "[" <> ref <> "]: " <>
                        content <> " " <>
                        "<a href=\"#ref-" <> ref <>
                        "\">&#8617;</a></p>"
    where
        ref = T.singleton num
        content = foldr ((<>) . emitHtml) T.empty text

mathExprGenerator :: MathExpr -> Text
mathExprGenerator (InlineExpr expr)  = "\\(" <> expr <> "\\)"
mathExprGenerator (BlockExpr expr) = "$$" <> expr <> "$$"

listItemGenerator :: [Element] -> Text
listItemGenerator text = "<li>" <> content <> "</li>\n"
    where content = foldr ((<>) . emitHtml) T.empty text

orderedListGenerator :: [Element] -> Text
orderedListGenerator items = "<ol>\n" <> content <> "</ol>"
    where content = foldr ((<>) . emitHtml) T.empty items

unorderedListGenerator :: [Element] -> Text
unorderedListGenerator items = "<ul>\n" <> content <> "</ul>"
    where content = foldr ((<>) . emitHtml) T.empty items

extractHeaderCols :: Element -> [Text]
extractHeaderCols (TableHeader cols) = cols
extractHeaderCols _ = []

tHeadGenerator :: Element -> Text
tHeadGenerator header = "<thead>\n<tr>\n" 
                      <> foldr ((<>) . fmtItem) T.empty (extractHeaderCols header)
                      <> "</tr>\n</thead>\n"
    where fmtItem item = "<th>" <> item <> "</th>\n"

extractRowCols :: Element -> [Text]
extractRowCols (TableRow cols) = cols
extractRowCols _ = []

tRowGenerator :: Element -> Text
tRowGenerator row = "<tr>\n"
                  <> foldr ((<>) . fmtItem) T.empty (extractRowCols row)
                  <> "</tr>\n"
    where fmtItem item = "<td>" <> item <> "</td>\n"

tBodyGenerator :: [Element] -> Text
tBodyGenerator rows = "<tbody>\n" <> foldr ((<>) . tRowGenerator) T.empty rows <> "</tbody>\n"

tableGenerator :: Element -> [Element] -> Text
tableGenerator header rows = "<table>\n"
                           <> tHeadGenerator header
                           <> tBodyGenerator rows
                           <> "</table>"

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
emitHtml (LItem text) = listItemGenerator text
emitHtml (OrderedList items) = orderedListGenerator items
emitHtml (UnorderedList items) = unorderedListGenerator items
emitHtml (TableHeader header) = tHeadGenerator (TableHeader header)
emitHtml (TableRow row) = tRowGenerator (TableRow row)
emitHtml (Table header rows) = tableGenerator header rows
emitHtml (Text text) = text