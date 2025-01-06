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

citParser :: Parser Element
citParser = do
  _   <- startToken
  _   <- newline
  Citation . T.pack <$> bodyParser
  where
    startToken = string "%C"
    bodyParser = manyTill anySingle endToken
    endToken   = string "C%"

textParser :: Parser Element
textParser = do
  text <- some (noneOf ['%'])
  return $ Text $ T.pack text

elementParser :: Parser [Element]
elementParser = many $ try boldParser
                <|> try italicParser
                <|> try linkParser
                <|> try picParser
                <|> try headParser
                <|> try icodeParser
                <|> try cbParser
                <|> try citParser
                <|> try textParser

emitHtml :: Element -> Text
emitHtml (Bold text) = "<b>" <> text <> "</b>"
emitHtml (Italic text) = "<i>" <> text <> "</i>"
emitHtml (Link text url) = "<a href=\"" <> url <> "\">" <> text <> "</a>"
emitHtml (Picture alt url) = "<img src=\"" <> url <> "\" alt=\"" <> alt <> "\">"
emitHtml (Header text) = "<h2>" <> text <> "</h2>"
emitHtml (ICode text) = "<code>" <> text <> "</code>"
emitHtml (CBlock lang content) = "<pre>\n<code class=\"language-" <> lang <> "\">\n" <> content <> "</code></pre>"
emitHtml (Citation cit) = "<blockquote>\n<div>></div>\n" <> cit <> "</blockquote>"
emitHtml (Text text) = text

converter :: Text -> Either ParserError Text
converter content = case parse elementParser "" content of
  Left err -> Left err
  Right elements -> Right $ foldr ((<>) . emitHtml) T.empty elements
