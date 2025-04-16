{-# LANGUAGE OverloadedStrings #-}
module EmitterTests where

import Test.HUnit ((~:), (~=?), Test(..) )
import Emitter (emitHtml)
import Types (Element(..))

testBoldEmitter :: Test
testBoldEmitter = TestList [
        "Bold" ~: "<b>bold text</b>" ~=? emitHtml (Bold [Text "bold text"])
    ]

testItalicEmitter :: Test
testItalicEmitter = TestList [
        "Italic" ~: "<i>italic text</i>" ~=? emitHtml (Italic [Text "italic text"])
    ]

testLinkEmitter :: Test
testLinkEmitter = TestList [
        "Link" ~: "<a href=\"http://example.com\">link</a>" ~=? emitHtml (Link [Text "link"] "http://example.com")
    ]

testPictureEmitter :: Test
testPictureEmitter = TestList [
        "Picture" ~: "<img class=\"post-img\" alt=\"alt\" src=\"http://example.com?pic.jpg\" width=\"800\" height=\"600\">"
            ~=? emitHtml (Picture "alt" "http://example.com?pic.jpg")
    ]

testHeaderEmitter :: Test
testHeaderEmitter = TestList [
        "Header" ~: "<h2 id=\"heading\" class=\"post-subtitle\">heading <a class=\"head-tag\" href=\"#heading\">§</a></h2>\n"
                 <> "<div class=\"sp\"></div>"
            ~=? emitHtml (Header [Text "heading"])
    ]

testICodeEmitter :: Test
testICodeEmitter = TestList [
        "inline code" ~: "<code class=\"inline-code\">snippet</code>"
            ~=? emitHtml (ICode "snippet")
    ]

testCBlockEmitter :: Test
testCBlockEmitter = TestList [
        "code block" ~: "<pre>\n<code class=\"language-lang\">\ncode</code></pre>"
            ~=? emitHtml (CBlock "lang" "code")
    ]

testCitationEmitter :: Test
testCitationEmitter = TestList [
        "citation" ~: "<blockquote>\n<div class=\"cursor\">></div>\ncitation\n</blockquote>"
            ~=? emitHtml (Citation [Text "citation"])
    ]

testRefLinkEmitter :: Test
testRefLinkEmitter = TestList [
        "ref link" ~: "<sup>[<a id=\"ref-1\" href=\"#foot-1\">1</a>]</sup>"
            ~=? emitHtml (RefLink '1')
    ]

testRefEmitter :: Test
testRefEmitter = TestList [
        "ref" ~: "<p id=\"foot-1\">[1]: reference <a href=\"#ref-1\">&#8617;</a></p>"
            ~=? emitHtml (Ref '1' [Text "reference"])
    ]

testIMathExprEmitter :: Test
testIMathExprEmitter = TestList [
        "inline math" ~: "\\(Expr\\)"
            ~=? emitHtml (IMathExpr "Expr")
    ]

testMathExprEmitter :: Test
testMathExprEmitter = TestList [
        "math block" ~: "$$Expr$$"
            ~=? emitHtml (MathExpr "Expr")
    ]

testListItemEmitter :: Test
testListItemEmitter = TestList [
        "list" ~: "<li>Foo</li>\n"
            ~=? emitHtml (LItem [Text "Foo"])
    ]

testOrderedListEmitter :: Test
testOrderedListEmitter = TestList [
        "ordered list" ~: "<ol>\n<li>One</li>\n<li>Two</li>\n<li>Three</li>\n</ol>"
            ~=? emitHtml ( OrderedList [ LItem [Text "One"]
                                       , LItem [Text "Two"]
                                       , LItem [Text "Three"]
                                       ])
    ]

testUnorderedListEmitter :: Test
testUnorderedListEmitter = TestList [
        "unordered list" ~: "<ul>\n<li>One</li>\n<li>Two</li>\n<li>Three</li>\n</ul>"
            ~=? emitHtml ( UnorderedList [ LItem [Text "One"]
                                         , LItem [Text "Two"]
                                         , LItem [Text "Three"]
                                         ])
    ]

testTableHeaderGenerator :: Test
testTableHeaderGenerator = TestList [
        "table header" ~: "<thead>\n<tr>\n<th>A</th>\n<th>B</th>\n<th>C</th>\n</tr>\n</thead>\n"
            ~=? emitHtml (TableHeader [Text "A", Text "B", Text "C"])
    ]

testTableRowGenerator :: Test
testTableRowGenerator = TestList [
        "table row" ~: "<tr>\n<td>F</td>\n<td>S</td>\n<td>T</td>\n<td>F</td>\n</tr>\n"
            ~=? emitHtml (TableRow [Text "F", Text "S", Text "T", Text "F"])
    ]

testTableGenerator :: Test
testTableGenerator = TestList [
        "table" ~: "<table>\n<thead>\n<tr>\n<th>A</th>\n<th>B</th>\n<th>C</th>\n<th>D</th>\n</tr>\n</thead>\n"
                 <> "<tbody>\n<tr>\n<td>F</td>\n<td>S</td>\n<td>T</td>\n<td>F</td>\n</tr>\n"
                 <> "<tr>\n<td>O</td>\n<td>T</td>\n<td>T</td>\n<td>F</td>\n</tr>\n"
                 <> "<tr>\n<td><b>I</b></td>\n<td>II</td>\n<td>III</td>\n<td>IV</td>\n</tr>\n"
                 <> "</tbody>\n</table>"
            ~=? emitHtml (Table
                            (TableHeader [Text "A", Text "B", Text "C", Text "D"])
                            [
                            TableRow [Text "F", Text "S", Text "T", Text "F"]
                            , TableRow [Text "O", Text "T", Text "T", Text "F"]
                            , TableRow [Bold [Text "I"], Text "II", Text "III", Text "IV"]
                            ]
                        )
    ]

emitterTests :: Test
emitterTests = TestList
    [ TestLabel "testBold" testBoldEmitter
    , TestLabel "testItalic" testItalicEmitter
    , TestLabel "testLink" testLinkEmitter
    , TestLabel "testPicture" testPictureEmitter
    , TestLabel "testHeader" testHeaderEmitter
    , TestLabel "testIcode" testICodeEmitter
    , TestLabel "testCBlock" testCBlockEmitter
    , TestLabel "testCitation" testCitationEmitter
    , TestLabel "testRefLink" testRefLinkEmitter
    , TestLabel "testReference" testRefEmitter
    , TestLabel "testIMathExpr" testIMathExprEmitter
    , TestLabel "testMathExpr" testMathExprEmitter
    , TestLabel "testListItem" testListItemEmitter
    , TestLabel "testOrderedList" testOrderedListEmitter
    , TestLabel "testUnorderedList" testUnorderedListEmitter
    , TestLabel "testTableHeader" testTableHeaderGenerator
    , TestLabel "testTableRow" testTableRowGenerator
    , TestLabel "testTable" testTableGenerator
    ]