{-# LANGUAGE OverloadedStrings #-}
module ParserTests where

import Test.HUnit
import Text.Megaparsec
import Parser
import Types (Element(..))

testBoldParser :: Test
testBoldParser = TestCase $ do
  let input = "%*Bold text%"
      expected = Bold [Text "Bold text"]
  case parse boldParser "" input of
    Left err  -> assertFailure $ "Parser error: " <> show err
    Right res -> assertEqual "Should parse bold text" expected res

testItalicParser :: Test
testItalicParser = TestCase $ do
  let input = "%_Italic text%"
      expected = Italic [Text "Italic text"]
  case parse italicParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse italic text" expected res

testLinkParser :: Test
testLinkParser = TestCase $ do
  let input = "%[link](http://example.com)%"
      expected = Link [Text "link"] ("http://example.com")
  case parse linkParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse link" expected res

testPicParser :: Test
testPicParser = TestCase $ do
  let input = "%![alt](http://example.com?pic.jpg)%"
      expected = Picture "alt" ("http://example.com?pic.jpg")
  case parse picParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse picture" expected res

testHeadParser :: Test
testHeadParser = TestCase $ do
  let input = "%#heading%"
      expected = Header [Text "heading"]
  case parse headParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse header" expected res

testICodeParser :: Test
testICodeParser = TestCase $ do
  let input = "%Isnippet%"
      expected = ICode "snippet"
  case parse icodeParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse inline code" expected res

testCBParser :: Test
testCBParser = TestCase $ do
  let input = "%Blang\ncodeB%"
      expected = CBlock "lang" "code"
  case parse cbParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse codeblock" expected res

testCitParser :: Test
testCitParser = TestCase $ do
  let input = "%Ccitation%"
      expected = Citation [Text "citation"]
  case parse citParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse citation" expected res

testRefLinkParser :: Test
testRefLinkParser = TestCase $ do
  let input = "%>1%"
      expected = RefLink '1'
  case parse refLinkParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse link to reference" expected res

testRefParser :: Test
testRefParser = TestCase $ do
  let input = "%<1reference%"
      expected = Ref '1' [Text "reference"]
  case parse refParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse reference" expected res

testIMathExprParser :: Test
testIMathExprParser = TestCase $ do
  let input = "%mExpr%"
      expected = IMathExpr "Expr"
  case parse imathExprParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse inline math expression" expected res

testMathExprParser :: Test
testMathExprParser = TestCase $ do
  let input = "%MExprM%"
      expected = MathExpr "Expr"
  case parse mathExprParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse math expression" expected res

testOListParser :: Test
testOListParser = TestCase $ do
  let input = "%OOne%\n%OTwo%%OThree%" -- Missing '\n' is intentional
      expected = OrderedList
        [ LItem [Text "One"]
        , LItem [Text "Two"]
        , LItem [Text "Three"]
        ]
  case parse oListParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse an ordered list" expected res

testSpecialCharacter :: Test
testSpecialCharacter = TestCase $ do
  let input = "%p%"
      expected = Text "%"
  case parse specialCharParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse the percentage character" expected res

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
  , TestLabel "testSpecialCharacter" testSpecialCharacter
  ]