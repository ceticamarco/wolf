module Engine where

import Args (Args(..))
import System.Directory (doesDirectoryExist, doesFileExist, createDirectoryIfMissing, listDirectory)
import System.FilePath ((</>))
import Control.Monad (unless)

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
        -- CHeck whether template file exists
        tpFileExists <- doesFileExist $ template args
        if not tpFileExists
          then return $ Just $ "ERROR: '" ++ template args ++ "' does not exist."
          else return Nothing

getDirContent :: FilePath -> IO [FilePath]
getDirContent dir = map (dir </>) <$> listDirectory dir

convertSrcFiles :: Args -> IO (Maybe String)
convertSrcFiles args = do
  -- Retrieve all files in the source directory
  fileList <- getDirContent $ srcDir args
  print fileList
  return Nothing
