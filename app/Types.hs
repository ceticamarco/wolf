{-# LANGUAGE StrictData #-}
module Types (Args(..), Element(..)) where

import Data.Text (Text)

data Args = Args
  { srcDir    :: FilePath
  , outDir    :: FilePath
  , template  :: FilePath
  , verbose   :: Bool
  } deriving(Show, Eq)


type Value = Text

data Element = Bold Value
             | Italic Value
             | Link Value Value
             | Picture Value Value
             | Header Value
             | ICode Value
             | CBlock Value Value
             | Citation Value
             | LRef Char
             | Ref Char Text
             | Text Value
             deriving (Show)
