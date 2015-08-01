module TranslationLister where

import Control.Monad ( forM )
import System.Directory ( getDirectoryContents, doesFileExist, doesDirectoryExist )
import System.FilePath ( (</>) )
import Data.List ( isSuffixOf, stripPrefix )

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

data TranslationLanguage = TranslationLanguage String | DefaultLanguage
                                                        deriving Show

allAfterLastElementOf :: Eq a => a -> [a] -> [a] -> [a]
allAfterLastElementOf _ [] acc = acc
allAfterLastElementOf el (x : xs) acc =
  if el == x then
    allAfterLastElementOf el xs []
  else
    allAfterLastElementOf el xs (acc ++ [x])

withoutSuffix :: Eq a => [a] -> [a] -> Maybe [a]
withoutSuffix suff = (fmap reverse) . (stripPrefix (reverse suff)) . reverse
                                                                 
resxFileLanguage :: FilePath -> TranslationLanguage
resxFileLanguage fp =
  let withoutResx = withoutSuffix ".resx" fp in
  case withoutResx of
    Nothing -> DefaultLanguage
    Just sp -> 
      let afterLastDot = allAfterLastElementOf '.' sp [] in
      if length afterLastDot > 6 then
        DefaultLanguage
      else
        if (elem '/' afterLastDot || elem '\\' afterLastDot) then
          DefaultLanguage
        else
          TranslationLanguage afterLastDot

       

