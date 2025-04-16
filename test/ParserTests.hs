{-# LANGUAGE OverloadedStrings #-}
module ParserTests where

import Test.HUnit ((~:), (~=?), Test(..) )
import Text.Megaparsec (parse)
import Parser
    ( boldParser,
      cbParser,
      citParser,
      headParser,
      icodeParser,
      imathExprParser,
      italicParser,
      linkParser,
      mathExprParser,
      oListParser,
      picParser,
      refLinkParser,
      refParser,
      specialCharParser,
      tableParser,
      uListParser )
import Types (Element(..))

testBoldParser :: Test
testBoldParser = TestList [
    "Normal bold" ~: Right (Bold [Text "text"]) ~=? parse boldParser "" "%*text%",
    "Recursive bold" ~: Right (Bold [Bold [Text "text"]]) ~=? parse boldParser "" "%*%*text%%"
  ]

testItalicParser :: Test
testItalicParser = TestList [
    "Normal italic" ~: Right (Italic [Text "text"]) ~=? parse italicParser "" "%_text%",
    "Recursive italic" ~: Right (Italic [Italic [Text "text"]]) ~=? parse italicParser "" "%_%_text%%"
  ]

testLinkParser :: Test
testLinkParser = TestList [
    "Normal link" ~: Right (Link [Text "link"] "http://example.com") ~=? parse linkParser "" "%[link](http://example.com)%",
    "Recursive link" ~: Right (Link [Bold [Text "link"]] "http://example.com") ~=? parse linkParser "" "%[%*link%](http://example.com)%"
  ]

testPicParser :: Test
testPicParser = TestList [
    "Normal pic" ~: Right (Picture "alt" "http://example.com?pic.jpg") ~=? parse picParser "" "%![alt](http://example.com?pic.jpg)%"
  ]

testHeadParser :: Test
testHeadParser = TestList [
    "Normal head" ~: Right (Header [Text "head"]) ~=? parse headParser "" "%#head%",
    "Recursive head" ~: Right (Header [Bold [Text "head"]]) ~=? parse headParser "" "%#%*head%%"
  ]

testICodeParser :: Test
testICodeParser = TestList [
    "Normal icode" ~: Right (ICode "snippet") ~=? parse icodeParser "" "%Isnippet%"
  ]

testCBParser :: Test
testCBParser = TestList [
    "Normal codeblock" ~: Right (CBlock "lang" "code") ~=? parse cbParser "" "%Blang\ncodeB%"
  ]

testCitParser :: Test
testCitParser = TestList [
    "Normal codeblock" ~: Right (Citation [Text "citation"]) ~=? parse citParser "" "%Ccitation%",
    "Recursive codeblock" ~: Right (Citation [Bold [Text "citation"]]) ~=? parse citParser "" "%C%*citation%%"
  ]

testRefLinkParser :: Test
testRefLinkParser = TestList [
    "Normal ref link" ~: Right (RefLink '1') ~=? parse refLinkParser "" "%>1%"
  ]

testRefParser :: Test
testRefParser = TestList [
    "Normal ref parser" ~: Right (Ref '1' [Text "ref"]) ~=? parse refParser "" "%<1ref%",
    "Recursive ref parser" ~: Right (Ref '1' [Bold [Text "ref"]]) ~=? parse refParser "" "%<1%*ref%%"
  ]

testIMathExprParser :: Test
testIMathExprParser = TestList [
    "Normal inline math" ~: Right (IMathExpr "Expr") ~=? parse imathExprParser "" "%mExpr%"
  ]

testMathExprParser :: Test
testMathExprParser = TestList [
    "Normal math expr" ~: Right (MathExpr "Expr") ~=? parse mathExprParser "" "%MExprM%"
  ]

testOListParser :: Test
testOListParser = TestList [
    "Normal ordered list" ~: Right (OrderedList [ LItem [Text "One"], LItem [Text "Two"], LItem [Text "Three"]])
      ~=? parse oListParser "" "%OOne%\n%OTwo%%OThree%",
    "Nested ordered list" ~: Right (OrderedList [ LItem [Bold [Text "One"]], LItem [Text "Two"], LItem [Text "Three"]])
      ~=? parse oListParser "" "%O%*One%%\n%OTwo%%OThree%"
  ]

testUListParser :: Test
testUListParser = TestList [
    "Normal unordered list" ~: Right (UnorderedList [ LItem [Text "One"], LItem [Text "Two"], LItem [Text "Three"]])
      ~=? parse uListParser "" "%UOne%\n%UTwo%%UThree%",
    "Nested unordered list" ~: Right (UnorderedList [ LItem [Bold [Text "One"]], LItem [Text "Two"], LItem [Text "Three"]])
      ~=? parse uListParser "" "%U%*One%%\n%UTwo%%UThree%"
  ]

testTableParser :: Test
testTableParser = TestList [
     "Normal table" ~: Right (Table (TableHeader [Text "A", Text "B", Text "C", Text "D"])
                                        [
                                          TableRow [Text "F", Text "S", Text "T", Text "F"]
                                        , TableRow [Text "O", Text "T", Text "T", Text "F"]
                                        , TableRow [Text "I", Text "II", Text "III", Text "IV"]
                                        ])
        ~=? parse tableParser "" "%T\nHA$B$C$D%\nRF$S$T$F%\nRO$T$T$F%RI$II$III$IV%%", -- Missing '\n' is intentional"
      "Nested table" ~: Right (Table (TableHeader [Text "A", Text "B", Text "C", Text "D"])
                                        [
                                          TableRow [Text "F", Bold [Text "S"], Text "T", Text "F"]
                                        , TableRow [Text "O", Text "T", Text "T", Text "F"]
                                        , TableRow [Text "I", Text "II", Text "III", Text "IV"]
                                        ])
        ~=? parse tableParser "" "%T\nHA$B$C$D%\nRF$%*S%$T$F%\nRO$T$T$F%RI$II$III$IV%%" -- Missing '\n' is intentional"
  ]


testSpecialChars :: Test
testSpecialChars = TestList [
      "percentage symbol" ~: Right (Text "%") ~=? parse specialCharParser "" "%p%",
      "dollar symbol" ~: Right (Text "$") ~=? parse specialCharParser "" "%$%"
  ]

parserTests :: Test
parserTests = TestList
  [ TestLabel "testBold" testBoldParser
  , TestLabel "testItalic" testItalicParser
  , TestLabel "testLink" testLinkParser
  , TestLabel "testPicture" testPicParser
  , TestLabel "testHeading" testHeadParser
  , TestLabel "testIcode" testICodeParser
  , TestLabel "testCBlock" testCBParser
  , TestLabel "testCitation" testCitParser
  , TestLabel "testRefLink" testRefLinkParser
  , TestLabel "testReference" testRefParser
  , TestLabel "testIMathExpression" testIMathExprParser
  , TestLabel "testMathExpression" testMathExprParser
  , TestLabel "testOrderedListParser" testOListParser
  , TestLabel "testUnorderedListParser" testUListParser
  , TestLabel "testTableParser" testTableParser
  , TestLabel "testSpecialCharacters" testSpecialChars
  ]