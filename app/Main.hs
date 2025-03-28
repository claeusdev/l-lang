module Main (main) where

import Parser (interpretFile)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      result <- interpretFile filename
      print result
    _ -> putStrLn "Usage: program <filename>"
