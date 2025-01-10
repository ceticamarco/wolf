module Main where
import Test.HUnit
import Text.Megaparsec
import qualified Data.Text as T
import Parser
import Types (Element(..))

testBoldParser :: Test
testBoldParser = TestCase $ do
  let input = T.pack "%*Bold text%"
      expected = Bold [Text (T.pack "Bold text")]
  case parse boldParser "" input of
    Left err  -> assertFailure $ "Parser error: " <> show err
    Right res -> assertEqual "Should parse bold text" expected res

testItalicParser :: Test
testItalicParser = TestCase $ do
  let input = T.pack "%_Italic text%"
      expected = Italic [Text (T.pack "Italic text")]
  case parse italicParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse italic text" expected res

testLinkParser :: Test
testLinkParser = TestCase $ do
  let input = T.pack "%[link](http://example.com)%"
      expected = Link [Text (T.pack "link")] (T.pack "http://example.com")
  case parse linkParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse link" expected res

testPicParser :: Test
testPicParser = TestCase $ do
  let input = T.pack "%![alt](http://example.com?pic.jpg)%"
      expected = Picture (T.pack "alt") (T.pack "http://example.com?pic.jpg")
  case parse picParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse picture" expected res

testHeadParser :: Test
testHeadParser = TestCase $ do
  let input = T.pack "%#heading%"
      expected = Header [Text (T.pack "heading")]
  case parse headParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse header" expected res

testICodeParser :: Test
testICodeParser = TestCase $ do
  let input = T.pack "%Isnippet%"
      expected = ICode (T.pack "snippet")
  case parse icodeParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse inline code" expected res

testCBParser :: Test
testCBParser = TestCase $ do
  let input = T.pack "%Blang\ncodeB%"
      expected = CBlock (T.pack "lang") (T.pack "code")
  case parse cbParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse codeblock" expected res

testCitParser :: Test
testCitParser = TestCase $ do
  let input = T.pack "%Ccitation%"
      expected = Citation [Text (T.pack "citation")]
  case parse citParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse citation" expected res

testRefLinkParser :: Test
testRefLinkParser = TestCase $ do
  let input = T.pack "%>1%"
      expected = RefLink '1'
  case parse refLinkParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse link to reference" expected res

testRefParser :: Test
testRefParser = TestCase $ do
  let input = T.pack "%<1reference%"
      expected = Ref '1' [Text (T.pack "reference")]
  case parse refParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse reference" expected res

testIMathExprParser :: Test
testIMathExprParser = TestCase $ do
  let input = T.pack "%mExpr%"
      expected = IMathExpr (T.pack "Expr")
  case parse imathExprParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse inline math expression" expected res

testMathExprParser :: Test
testMathExprParser = TestCase $ do
  let input = T.pack "%MExprM%"
      expected = MathExpr (T.pack "Expr")
  case parse mathExprParser "" input of
    Left err   -> assertFailure $ "Parser error: " <> show err
    Right res  -> assertEqual "Should parse math expression" expected res

tests :: Test
tests = TestList
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
  ]

main :: IO ()
main = runTestTTAndExit tests