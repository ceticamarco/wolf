{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Types (Element(..))

type Parser = Parsec Void Text
type ParserError = ParseErrorBundle Text Void

-- Bold text is defined as '%*Bold text%'
boldParser :: Parser Element
boldParser = do
  _    <- startToken
  text <- some nonToken
  _    <- endToken
  return $ Bold $ T.pack text
  where
    startToken = string "%*"
    nonToken   = noneOf ['%']
    endToken   = string "%"

-- Italic text is defined as '%_Italic text%'
italicParser :: Parser Element
italicParser = do
  _    <- startToken
  text <- some nonToken
  _    <- endToken
  return $ Italic $ T.pack text
  where
    startToken = string "%_"
    nonToken   = noneOf ['%']
    endToken   = string "%"

-- Links are defined as '%[link text](url)%
linkParser :: Parser Element
linkParser = do
  _    <- startToken
  text <- linkTextParser
  link <- urlParser
  _    <- endToken
  return $ Link (T.pack text) (T.pack link)
  where
    startToken     = "%"
    endToken       = startToken
    linkTextParser = between (char '[') (char ']') (some (noneOf [']']))
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
  text <- some nonToken
  _    <- endToken
  return $ Header (T.pack text)
  where
    startToken = string "%#"
    nonToken   = noneOf ['%']
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
  _       <- newline
  CBlock (T.pack lang) . T.pack <$> bodyParser
  where
    startToken     = string "%B"
    langNameParser = some letterChar
    bodyParser     = manyTill anySingle endToken
    endToken       = string "B%"

-- Citations are defined as '%C citationC%'
citParser :: Parser Element
citParser = do
  _   <- startToken
  _   <- newline
  Citation . T.pack <$> bodyParser
  where
    startToken = string "%C"
    bodyParser = manyTill anySingle endToken
    endToken   = string "C%"

-- Links to footnotes are defined as '%>NUM%'
linkRefParser :: Parser Element
linkRefParser = do
  _      <- startToken
  refNum <- digitChar
  _      <- endToken
  return $ LRef refNum
  where
    startToken = string "%>"
    endToken   = string "%"

-- Footnotes are defined as '%<NUM FOOTNOTE%'
refParser :: Parser Element
refParser = do
  _      <- startToken
  refNum <- digitChar
  ref    <- some nonToken
  _      <- endToken
  return $ Ref refNum (T.pack ref)
  where
    startToken = string "%<"
    nonToken   = noneOf ['%']
    endToken   = string "%"

-- Parses any non token
textParser :: Parser Element
textParser = do
  text <- some (noneOf ['%'])
  return $ Text $ T.pack text

-- Failback parser for syntax erorrs
failParser :: Parser Element
failParser = do
  _ <- anySingle
  fail "Unexpected token"

elementParser :: Parser [Element]
elementParser = many $ try boldParser
                <|> try italicParser
                <|> try linkParser
                <|> try picParser
                <|> try headParser
                <|> try icodeParser
                <|> try cbParser
                <|> try citParser
                <|> try linkRefParser
                <|> try refParser
                <|> try textParser
                <|> failParser

emitHtml :: Element -> Text
emitHtml (Bold text) = "<b>" <> text <> "</b>"
emitHtml (Italic text) = "<i>" <> text <> "</i>"
emitHtml (Link text url) = "<a href=\"" <> url <> "\">" <> text <> "</a>"
emitHtml (Picture alt url) = "<img src=\"" <> url <> "\" alt=\"" <> alt <> "\">"
emitHtml (Header text) = "<h2>" <> text <> "</h2>"
emitHtml (ICode text) = "<code>" <> text <> "</code>"
emitHtml (CBlock lang content) = "<pre>\n<code class=\"language-" <> lang <> "\">\n" <> content <> "</code></pre>"
emitHtml (Citation cit) = "<blockquote>\n<div>></div>\n" <> cit <> "</blockquote>"
emitHtml (LRef num) = "<a id=\"ref-" <> T.singleton num <> "\" href=\"#foot-" <> T.singleton  num <> "\">[" <> T.singleton num <> "]</a>"
emitHtml (Ref num ref) = "<p id=\"foot-" <> T.singleton num <> "\">[" <> T.singleton num <> "]: " <> ref <> " <a href=\"#ref-" <> T.singleton num <> "\">&#8617;</a></p>"
emitHtml (Text text) = text

converter :: Text -> Either ParserError Text
converter content = case parse elementParser "" content of
  Left err -> Left err
  Right elements -> Right $ foldr ((<>) . emitHtml) T.empty elements
