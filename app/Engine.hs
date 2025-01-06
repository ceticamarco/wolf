{-# LANGUAGE OverloadedStrings #-}

module Engine where

import Types (Args(..))
import Parser (converter)
import System.Directory (doesDirectoryExist, doesFileExist, createDirectoryIfMissing, listDirectory)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromMaybe)
import Data.Either (fromRight)
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
                        
convertFile :: Bool -> FilePath -> FilePath -> FilePath -> IO ()
convertFile is_verbose outputDir tplFile srcFile = do
  -- Print a status message
  when is_verbose $ putStrLn $ "Publishing " ++ srcFile ++ "..."
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

  -- Convert custom language to HTML (TODO: PROPAGATE ERRORS TO MAIN)
  let convertedFile = fromRight "" (converter srcFileContent)

  -- Retrieve post metadata
  metadata <- getFileMetadata srcFile

  -- Remove metadata from source file(i.e., first 5 lines)
  let wholeFile = T.lines convertedFile
      postContent = T.unlines $ drop 5 wholeFile

  -- Double escape backslash characters
  let escPostContent = T.replace "\\" "\\\\" postContent

  -- Replace page content into template file
  let templateWithContent = T.replace "%%CONTENT%%" escPostContent tplFileContent

  -- Replace metadata placeholders with actual values
  let postWithMetadata = fillMetadata templateWithContent metadata

  -- Add timestamp and build information
  timestamp <- getTimestamp
  let finalPost = T.replace "%%TIMESTAMP%%" (T.pack info) postWithMetadata
       where info = "\t<!--\n\tGenerated with Rhino Template Engine\n"
                 <> "\tDeveloped by Marco Cetica\n"
                 <> "\tTimestamp: " <> timestamp <> "-->"

  -- Write converted post to the output file
  TIO.writeFile outputFile finalPost

convertFiles :: Args -> IO ()
convertFiles args = do
  -- Retrieve all files in the source directory
  fileList <- getDirContent $ srcDir args
  -- Convert all source files in the source directory
  mapM_ (convertFile v o t) fileList
  where
    v = verbose args
    o = outDir args
    t = template args
