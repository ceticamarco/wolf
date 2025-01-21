{-# LANGUAGE OverloadedStrings #-}
module EmitterTests where

import Test.HUnit
import Emitter (emitHtml)
import Types (Element(..))

testBoldEmitter :: Test
testBoldEmitter = TestCase $ do
    let input = Bold [Text ("bold text")]
        expected = "<b>bold text</b>"
        actual = emitHtml input
    assertEqual "Should emit bold text" expected actual

testItalicEmitter :: Test
testItalicEmitter = TestCase $ do
    let input = Italic [Text ("italic text")]
        expected = "<i>italic text</i>"
        actual = emitHtml input
    assertEqual "Should emit italic text" expected actual

testLinkEmitter :: Test
testLinkEmitter = TestCase $ do
    let input = Link [Text ("link")] ("http://example.com")
        expected = "<a href=\"http://example.com\">link</a>"
        actual = emitHtml input
    assertEqual "Should emit a link" expected actual

testPictureEmitter :: Test
testPictureEmitter = TestCase $ do
    let input = Picture ("alt") ("http://example.com?pic.jpg")
        expected = "<img class=\"post-img\" alt=\"alt\" " 
                 <> "src=\"http://example.com?pic.jpg\" "
                 <> "width=\"800\" height=\"600\">"
        actual = emitHtml input
    assertEqual "Should emit a picture" expected actual

testHeaderEmitter :: Test
testHeaderEmitter = TestCase $ do
    let input = Header [Text "heading"]
        expected = "<h2 id=\"heading\" class=\"post-subtitle\">heading "
                <> "<a class=\"head-tag\" href=\"#heading\">§</a></h2>\n"
                <> "<div class=\"sp\"></div>"
        actual = emitHtml input
    assertEqual "Should emit an header" expected actual
    
testICodeEmitter :: Test
testICodeEmitter = TestCase $ do
    let input = ICode "snippet"
        expected = "<code class=\"inline-code\">snippet</code>"
        actual = emitHtml input
    assertEqual "Should emit an inline code snippet" expected actual

testCBlockEmitter :: Test
testCBlockEmitter = TestCase $ do
    let input = CBlock "lang" "code"
        expected = "<pre>\n<code class=\"language-lang\">\ncode</code></pre>"
        actual = emitHtml input
    assertEqual "Should emit a code block" expected actual

testCitationEmitter :: Test
testCitationEmitter = TestCase $ do
    let input = Citation [Text "citation"]
        expected = "<blockquote>\n<div class=\"cursor\">></div>\ncitation\n</blockquote>"
        actual = emitHtml input
    assertEqual "Should emit a blockquote" expected actual

testRefLinkEmitter :: Test
testRefLinkEmitter = TestCase $ do
    let input = RefLink '1'
        expected = "<a id=\"ref-1\" href=\"#foot-1\">[1]</a>"
        actual = emitHtml input
    assertEqual "Should emit a link to reference" expected actual

testRefEmitter :: Test
testRefEmitter = TestCase $ do
    let input = Ref '1' [Text "reference"]
        expected = "<p id=\"foot-1\">[1]: reference "
                <> "<a href=\"#ref-1\">&#8617;</a></p>"
        actual = emitHtml input
    assertEqual "Should emit a reference" expected actual

testIMathExprEmitter :: Test
testIMathExprEmitter = TestCase $ do
    let input = IMathExpr "Expr"
        expected = "\\(Expr\\)"
        actual = emitHtml input
    assertEqual "Should emit an inline math expression" expected actual

testMathExprEmitter :: Test
testMathExprEmitter = TestCase $ do
    let input = MathExpr "Expr"
        expected = "$$Expr$$"
        actual = emitHtml input
    assertEqual "Should emit a math expression" expected actual

testListItemEmitter :: Test
testListItemEmitter = TestCase $ do
    let input = LItem [Text "Foo"]
        expected = "<li>Foo</li>\n"
        actual = emitHtml input
    assertEqual "Should emit a list item" expected actual

testOrderedListEmitter :: Test
testOrderedListEmitter = TestCase $ do
    let input = OrderedList
            [ LItem [Text "One"]
            , LItem [Text "Two"]
            , LItem [Text "Three"]
            ]
        expected = "<ol>\n"
                <> "<li>One</li>\n"
                <> "<li>Two</li>\n"
                <> "<li>Three</li>\n"
                <> "</ol>"
        actual = emitHtml input
    assertEqual "Should emit an ordered list" expected actual

testUnorderedListEmitter :: Test
testUnorderedListEmitter = TestCase $ do
    let input = UnorderedList
            [ LItem [Text "One"]
            , LItem [Text "Two"]
            , LItem [Text "Three"]
            ]
        expected = "<ul>\n"
                <> "<li>One</li>\n"
                <> "<li>Two</li>\n"
                <> "<li>Three</li>\n"
                <> "</ul>"
        actual = emitHtml input
    assertEqual "Should emit an unordered list" expected actual

testTableHeaderGenerator :: Test
testTableHeaderGenerator = TestCase $ do
    let input = TableHeader ["A", "B", "C"]
        expected = "<thead>\n<tr>\n<th>A</th>\n<th>B</th>\n<th>C</th>\n</tr>\n</thead>\n"
        actual = emitHtml input
    assertEqual "Should emit a table header" expected actual

testTableRowGenerator :: Test
testTableRowGenerator = TestCase $ do
    let input = TableRow ["F", "S", "T", "F"]
        expected = "<tr>\n<td>F</td>\n<td>S</td>\n<td>T</td>\n<td>F</td>\n</tr>\n"
        actual = emitHtml input
    assertEqual "Should emit a table row" expected actual

testTableGenerator :: Test
testTableGenerator = TestCase $ do
    let input = Table
            (TableHeader ["A", "B", "C", "D"])
            [
            TableRow ["F", "S", "T", "F"]
            , TableRow ["O", "T", "T", "F"]
            , TableRow ["I", "II", "III", "IV"]
            ]
        expected = "<table>\n<thead>\n<tr>\n<th>A</th>\n<th>B</th>\n<th>C</th>\n<th>D</th>\n</tr>\n</thead>\n"
                 <> "<tbody>\n<tr>\n<td>F</td>\n<td>S</td>\n<td>T</td>\n<td>F</td>\n</tr>\n"
                 <> "<tr>\n<td>O</td>\n<td>T</td>\n<td>T</td>\n<td>F</td>\n</tr>\n"
                 <> "<tr>\n<td>I</td>\n<td>II</td>\n<td>III</td>\n<td>IV</td>\n</tr>\n"
                 <> "</tbody>\n</table>"
        actual = emitHtml input
    assertEqual "Should emit a table" expected actual

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