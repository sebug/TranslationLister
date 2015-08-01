module Main where

import System.Environment ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: TranslationLister sourceDirectory"
    (dir : _) -> putStrLn dir

