module Main where

import Control.Monad ( forM )
import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode( ExitFailure ) )
import System.IO ( hPutStrLn, stderr )
import System.Directory ( getDirectoryContents, doesFileExist, doesDirectoryExist )
import System.FilePath

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

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
          hPutStrLn stderr "Usage: TranslationLister sourceDirectory"
          exitWith (ExitFailure 1)
    (dir : _) -> do
      cts <- getRecursiveContents dir
      putStrLn (show $ cts)


