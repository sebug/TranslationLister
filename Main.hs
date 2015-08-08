module Main where

import System.Environment ( getArgs )
import System.IO ( hPutStrLn, stderr )
import System.Exit ( exitWith, ExitCode( ExitFailure ) )

import Text.XML.HXT.Parser.XmlParsec ( xreadDoc )
import Text.XML.HXT.DOM.TypeDefs ( XmlTrees, XmlTree, XNode(..) )
import Text.XML.HXT.DOM.QualifiedName ( localPart )
import Data.Tree.NTree.TypeDefs ( NTree(..) )

import TranslationLister ( getResxFiles, resxFileGroups, resxFileLanguage )

type TranslationEntry = (String,String)

extract_entries :: XmlTree -> [TranslationEntry]
extract_entries tree = []

is_root :: XmlTree -> Bool
is_root (NTree (XTag rootqname _) _) = localPart rootqname == "root"
is_root _ = False

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
            let roots = filter is_root contentAsXml in
            case roots of
              [] -> putStrLn "No entries found"
              [r] -> putStrLn (show r)
              _ -> putStrLn "Too many roots found"



