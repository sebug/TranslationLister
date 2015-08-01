module TranslationLister where

import Control.Monad ( forM )
import System.Directory ( getDirectoryContents, doesFileExist, doesDirectoryExist )
import System.FilePath ( (</>) )
import Data.List ( isSuffixOf )

-- Directly lifted from http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

getResxFiles :: FilePath -> IO [FilePath]
getResxFiles topdir = do
  files <- getRecursiveContents topdir
  let resxs = filter (isSuffixOf ".resx") files
  return resxs

