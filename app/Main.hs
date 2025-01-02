module Main where

import Options.Applicative

data Args = Args
  { srcDir    :: String
  , outputDir :: String
  , template  :: String
  , verbose   :: Bool
  }

argsParser :: Parser Args
argsParser = Args
  <$> strOption
      ( long "src"
     <> short 's'
     <> metavar "SRC_DIR"
     <> help "Specify source directory" )
  <*> strOption
      ( long "output"
     <> short 'o'
     <> metavar "OUT_DIR"
     <> help "Specify output directory" )
  <*> strOption
      ( long "template"
     <> short 't'
     <> metavar "TEMPLATE"
     <> help "Specify template file")
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "Enable verbose mode")

main :: IO ()
main = do
  args <- execParser opts
  -- TODO: do something with CLI arguments store into 'args'
  where
    opts = info (argsParser <**> helper)
      ( fullDesc
     <> progDesc "rhino - markup language for building static websites"
     <> header "rhino v0.1 by Marco Cetica (c) 2025")
