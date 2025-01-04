module Types (Args(..)) where

data Args = Args
  { srcDir    :: !FilePath
  , outDir    :: !FilePath
  , template  :: !FilePath
  , verbose   :: !Bool
  } deriving(Show, Eq)
