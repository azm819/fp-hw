module Lib ( someFunc ) where

import Parser
import Runner
import System.Environment
import System.IO

someFunc :: IO ()
someFunc = do
  [filename] <- getArgs
  s <- readFile filename
  case parse filename s of
    Left err -> putStrLn err
    Right prog -> do
      print prog
      run prog
