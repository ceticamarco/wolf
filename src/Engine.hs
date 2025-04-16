{-# LANGUAGE OverloadedStrings #-}
module Engine where

import Types (Args(..))
import Parser (converter)
import Paths_rhino (version)
import Data.Version (showVersion)
import System.Directory (doesDirectoryExist, doesFileExist, createDirectoryIfMissing, listDirectory)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Data.Text (Text)
import Text.Megaparsec (errorBundlePretty)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromMaybe, catMaybes, listToMaybe)
import System.FilePath ((</>), takeBaseName)
import Control.Monad (unless, when)

checkPaths :: Args -> IO (Maybe String)
checkPaths args = do
  -- Check whether source directory exists
  srcDirExists <- doesDirectoryExist $ srcDir args
  if not srcDirExists
    then return $ Just $ "ERROR: '" ++ srcDir args ++ "' does not exist."
    else do
        -- Check whether output directory exists. If not, create it
        outDirExists <- doesDirectoryExist $ outDir args
        unless outDirExists $ createDirectoryIfMissing True $ outDir args
        -- Check whether template file exists
        tpFileExists <- doesFileExist $ template args
        if not tpFileExists
          then return $ Just $ "ERROR: '" ++ template args ++ "' does not exist."
          else return Nothing

getDirContent :: FilePath -> IO [FilePath]
getDirContent dir = map (dir </>) <$> listDirectory dir

processLine :: Text -> Maybe (Text, Text)
processLine line = do
  strippedLine <- T.stripPrefix "#+" line
  let (key, rest) = T.breakOn ":" strippedLine
  value <- T.stripPrefix ":" rest
  return (T.strip key, T.strip value)

checkMetadata :: FilePath -> IO Bool
checkMetadata srcFile = do
  content <- TIO.readFile srcFile
  let patterns = [ "#+HEAD_TITLE:"
                 , "#+TITLE:"
                 , "#+DESCRIPTION:"
                 , "#+DATE:"
                 , "#+TAGS:"
                 ]
  let contentList = take 5 $ T.lines content
  if length contentList < 5
    then return False
    else do
      let header = zip contentList patterns
      return $ all (\(line, prefix) -> T.isPrefixOf prefix line) header

getFileMetadata :: FilePath -> IO [(Text, Text)]
getFileMetadata srcFile = do
  content <- TIO.readFile srcFile
  let header = take 5 $ T.lines content
  let metadata = map (fromMaybe (T.empty, T.empty) . processLine) header
  return metadata

getTimestamp :: IO String
getTimestamp = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

fillMetadata :: T.Text -> [(Text, Text)] -> T.Text
fillMetadata content [] = content
fillMetadata content ((placeholder, value):xs) =
    fillMetadata (T.replace expr value content) xs
  where
    expr = "%%" <> placeholder <> "%%"

convertFile :: Bool -> FilePath -> FilePath -> FilePath -> IO (Maybe String)
convertFile is_verbose outputDir tplFile srcFile = do
  -- Print a status message
  when is_verbose $ putStrLn $ "Publishing " <> srcFile <> "..."
  -- Create post folder in '<OUTPUT_DIR>/<POST_DIRNAME>/index.html'
  -- if <SOURCE_FILE> != 'index'
  let postDirName = takeBaseName srcFile
      outputFile  = if postDirName /= "index"
                    then outputDir </> postDirName </> "index.html"
                    else outputDir </> "index.html"
  when (postDirName /= "index") $ createDirectoryIfMissing True (outputDir </> postDirName)

  -- Read source file and template
  srcFileContent <- TIO.readFile srcFile
  tplFileContent <- TIO.readFile tplFile

  -- Check whether post metadata is specified
  isHeaderValid <- checkMetadata srcFile
  if not isHeaderValid
    then return $ Just $ "Please, specify the header of '" <> srcFile <> "'"
    else
      -- Convert file to HTML
      case converter srcFileContent of
        Left err -> return $ Just $ "Error processing file '"
                    <> srcFile <> "' @ " <> errorBundlePretty err
        Right convertedFile -> do
          -- Retrieve post metadata
          metadata <- getFileMetadata srcFile

          -- Remove metadata from source file(i.e., first 5 lines)
          let wholeFile = T.lines convertedFile
              postContent = T.unlines $ drop 5 wholeFile

          -- Replace page content into template file
          let templateWithContent = T.replace "%%CONTENT%%" postContent tplFileContent

          -- Replace metadata placeholders with actual values
          let postWithMetadata = fillMetadata templateWithContent metadata

          -- Add timestamp and build information
          timestamp <- getTimestamp
          let info = "\t<!--\n\tPowered by Rhino Template Engine(v" 
                  <> showVersion version <> ")\n"
                  <> "\tDeveloped by Marco Cetica\n"
                  <> "\tTimestamp: " <> timestamp <> "-->"
              finalPost = T.replace "%%TIMESTAMP%%" (T.pack info) postWithMetadata

          -- Write converted post to the output file
          TIO.writeFile outputFile finalPost
          return Nothing

convertFiles :: Args -> IO (Maybe String)
convertFiles args = do
  -- Retrieve all files in the source directory
  fileList <- getDirContent $ srcDir args
  -- Convert all source files in the source directory
  res <- mapM (convertFile v o t) fileList
  let errors = catMaybes res
  return $ listToMaybe errors
  where
    v = verbose args
    o = outDir args
    t = template args
