module Main where

import System.Environment ( getArgs )
import System.IO ( hPutStrLn, stderr )
import System.Exit ( exitWith, ExitCode( ExitFailure ) )

import TranslationLister ( getResxFiles, resxFileGroups, resxFileLanguage )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
          hPutStrLn stderr "Usage: TranslationLister sourceDirectory"
          exitWith (ExitFailure 1)
    (dir : _) -> do
      cts <- getResxFiles dir
      let resxGroups = resxFileGroups cts in
        let groupsWithTranslations = filter (\l -> length l > 1) resxGroups in
        putStrLn (show groupsWithTranslations)


