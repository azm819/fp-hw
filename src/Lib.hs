module Lib ( someFunc ) where

import Lexer
import Parser

someFunc :: IO ()
someFunc = do
  s <- getContents
  case parse "filename.py" s of
    Left err -> putStrLn err
    Right prog -> print prog
