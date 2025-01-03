module Engine where

import Types (Args(..))
import System.Directory (doesDirectoryExist, doesFileExist, createDirectoryIfMissing, listDirectory)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromMaybe)
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
  strippedLine <- T.stripPrefix (T.pack "#+") line
  let (key, rest) = T.breakOn (T.pack ":") strippedLine
  value <- T.stripPrefix (T.pack ":") rest
  return (T.strip key, T.strip value)

getFileMetadata :: FilePath -> IO ()
getFileMetadata srcFile = do
  content <- TIO.readFile srcFile
  let header = take 5 $ T.lines content
  let metadata = map (fromMaybe (T.empty, T.empty) . processLine) header
  print metadata
                        
convertFile :: Bool -> FilePath -> FilePath -> IO ()
convertFile is_verbose outputDir srcFile = do
  -- Print a status message
  when is_verbose $ putStrLn $ "Publishing " ++ srcFile ++ "..."
  -- Create post folder in '<OUTPUT_DIR>/<POST_DIRNAME>/index.html'
  -- if <SOURCE_FILE> != 'index'
  let postDirName = takeBaseName srcFile
      outputFile  = if postDirName /= "index"
                    then outputDir </> postDirName </> "index.html"
                    else outputDir </> "index.html"    
  when (postDirName /= "index") $ createDirectoryIfMissing True (outputDir </> postDirName)

  -- Retrieve post metadata
  getFileMetadata srcFile
  print outputFile

convertFiles :: Args -> IO ()
convertFiles args = do
  -- Retrieve all files in the source directory
  fileList <- getDirContent $ srcDir args
  -- Convert all source files in the source directory
  mapM_ (convertFile (verbose args) (outDir args)) fileList
