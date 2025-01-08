{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char ( char, digitChar, newline, string )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Types (Element(..))
import Emitter (emitHtml)

type Parser = Parsec Void Text
type ParserError = ParseErrorBundle Text Void

-- Bold text is defined as '%*Bold text%'
boldParser :: Parser Element
boldParser = do
  _    <- startToken
  text <- bodyParser
  _    <- endToken
  return $ Bold text
  where
    startToken = string "%*"
    bodyParser = many (try nestedElementParser)
    endToken   = string "%"

-- Italic text is defined as '%_Italic text%'
italicParser :: Parser Element
italicParser = do
  _    <- startToken
  text <- bodyParser
  _    <- endToken
  return $ Italic text
  where
    startToken = string "%_"
    bodyParser = many (try nestedElementParser)
    endToken   = string "%"

-- Links are defined as '%[link text](url)%
linkParser :: Parser Element
linkParser = do
  _    <- startToken
  text <- linkTextParser
  link <- urlParser
  _    <- endToken
  return $ Link text (T.pack link)
  where
    startToken     = "%"
    endToken       = startToken
    linkTextParser = between (char '[') (char ']') (many (try nestedElementParser))
    urlParser      = between (char '(') (char ')') (some (noneOf [')']))

-- Images are defined as '%![alt text](url)%'
picParser :: Parser Element
picParser = do
  _    <- startToken
  alt  <- altTextParser
  url  <- urlParser
  _    <- endToken
  return $ Picture (T.pack alt) (T.pack url)
  where
    startToken    = string "%!"
    endToken      = string "%"
    altTextParser = between (char '[') (char ']') (some (noneOf [']']))
    urlParser     = between (char '(') (char ')') (some (noneOf [')']))

-- Headers are defined as '%#Header title%'
headParser :: Parser Element
headParser = do
  _    <- startToken
  text <- bodyParser
  _    <- endToken
  return $ Header text
  where
    startToken = string "%#"
    bodyParser = many (try nestedElementParser)
    endToken   = string "%"

-- Inline code is defined as '%Icode snippet%''
icodeParser :: Parser Element
icodeParser = do
  _    <- startToken
  text <- some nonToken
  _    <- endToken
  return $ ICode (T.pack text)
  where
    startToken = string "%I"
    nonToken   = noneOf ['%']
    endToken   = string "%"

-- Codeblocks are defined as '%B<SYNTAX> code snippetB%'
cbParser :: Parser Element
cbParser = do
  _       <- startToken
  lang    <- langNameParser
  CBlock (T.pack lang) . T.pack <$> bodyParser
  where
    startToken     = string "%B"
    langNameParser = manyTill anySingle newline
    bodyParser     = manyTill anySingle endToken
    endToken       = string "B%"

-- Citations are defined as '%Ccitation%'
citParser :: Parser Element
citParser = do
  _    <- startToken
  body <- bodyParser
  _    <- endToken
  return $ Citation body
  where
    startToken = string "%C"
    bodyParser = many (try nestedElementParser)
    endToken   = string "%"

-- Links to footnotes are defined as '%>NUM%'
refLinkParser :: Parser Element
refLinkParser = do
  _      <- startToken
  refNum <- digitChar
  _      <- endToken
  return $ RefLink refNum
  where
    startToken = string "%>"
    endToken   = string "%"

-- Footnotes are defined as '%<NUM FOOTNOTE%'
refParser :: Parser Element
refParser = do
  _      <- startToken
  refNum <- digitChar
  ref    <- many (try nestedElementParser)
  _      <- endToken
  return $ Ref refNum ref
  where
    startToken = string "%<"
    endToken   = string "%"

-- Inline LaTeX expressions are defined as '%mEXPRESSION%'
imathExprParser :: Parser Element
imathExprParser = do
  _    <- startToken
  expr <- some nonToken
  _    <- endToken
  return $ IMathExpr $ T.pack expr
  where
    startToken = string "%m"
    nonToken   = noneOf ['%']
    endToken   = string "%"

-- LaTeX expressions are defined as '%M EXORESSION M%'
mathExprParser :: Parser Element
mathExprParser = do
  _    <- startToken
  MathExpr . T.pack <$> bodyParser
  where
    startToken = string "%M"
    bodyParser = manyTill anySingle endToken
    endToken   = string "M%"
    

-- Parses any non token
textParser :: Parser Element
textParser = do
  text <- some (noneOf ['%', '[', ']'])
  return $ Text (T.pack text)

-- Failback parser for syntax erorrs
failParser :: Parser Element
failParser = do
  _ <- anySingle
  fail "Unexpected token"


-- Nested parser to handle language elements
nestedElementParser :: Parser Element
nestedElementParser =
  try boldParser <|> try italicParser                           -- Formatting parsers
  <|> try linkParser <|> try picParser                          -- Link parsers
  <|> try headParser <|> try icodeParser                        --
  <|> refLinkParser  <|> try imathExprParser                    --
  <|> try citParser  <|> try refParser                          -- Inline element parsers
  <|> try cbParser   <|> try mathExprParser                     -- Block element parsers
  <|> try textParser <|> failParser                             -- Generic parsers

-- Top level syntax parser
elementParser :: Parser [Element]
elementParser = many nestedElementParser

converter :: Text -> Either ParserError Text
converter content = case parse elementParser "" content of
  Left err -> Left err
  Right elements -> Right $ foldr ((<>) . emitHtml) T.empty elements
