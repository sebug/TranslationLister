module Main where

import System.Environment ( getArgs )
import System.IO ( hPutStrLn, stderr )
import System.Exit ( exitWith, ExitCode( ExitFailure ) )

import Text.XML.HXT.Parser.XmlParsec

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
        let (veryFirstPath,_) = head $ head groupsWithTranslations in
        do
          content <- readFile veryFirstPath
          let contentWithoutBom = tail content in
            let contentAsXml = xreadDoc contentWithoutBom in
            putStrLn (show contentAsXml)


