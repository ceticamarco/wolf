{-# LANGUAGE StrictData #-}
module Types (Args(..), Element(..)) where

import Data.Text (Text)

data Args = Args
  { srcDir    :: FilePath
  , outDir    :: FilePath
  , template  :: FilePath
  , verbose   :: Bool
  }

type Value = Text
data Element = Bold [Element]
             | Italic [Element]
             | Link [Element] Value
             | Picture Value Value
             | Header [Element]
             | ICode Value
             | CBlock Value Value
             | Citation [Element]
             | RefLink Char
             | Ref Char [Element]
             | IMathExpr Value
             | MathExpr Value
             | Text Value
