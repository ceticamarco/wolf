module Types (Args(..)) where

data Args = Args
  { srcDir    :: String
  , outDir :: String
  , template  :: String
  , verbose   :: Bool
  } deriving(Show, Eq)
