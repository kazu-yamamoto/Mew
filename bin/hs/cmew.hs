module Main where

import Mail
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  cs <- readFile file
  print $ parseMail cs file
