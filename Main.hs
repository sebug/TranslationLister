module Main where

import System.Environment ( getArgs )
import System.IO ( hPutStrLn, stderr )
import System.Exit ( exitWith, ExitCode( ExitFailure ) )

import TranslationLister ( getResxFiles )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
          hPutStrLn stderr "Usage: TranslationLister sourceDirectory"
          exitWith (ExitFailure 1)
    (dir : _) -> do
      cts <- getResxFiles dir
      putStrLn (show $ cts)


