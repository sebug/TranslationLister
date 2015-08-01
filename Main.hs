module Main where

import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode( ExitFailure ) )
import System.IO ( hPutStrLn, stderr )
import System.Directory ( getDirectoryContents )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
          hPutStrLn stderr "Usage: TranslationLister sourceDirectory"
          exitWith (ExitFailure 1)
    (dir : _) -> do
      cts <- getDirectoryContents dir
      putStrLn (show $ head cts)


