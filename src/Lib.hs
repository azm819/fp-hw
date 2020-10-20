module Lib ( someFunc ) where

import AST
import Parser
import Runner
import System.Environment
import System.IO

main' :: IO ()
main' = do
  [filename] <- getArgs
  s <- readFile filename
  case parse filename s of
    Left err -> putStrLn err
    Right prog -> do
      print prog
      run prog

someFunc :: IO ()
someFunc = putStr $ showProg $ edsl $ do
  "n" $=$ i 0
  while (n "n" $<$ i 100) $ do
    if' (n "n" $%$ i 15 $==$ i 0) $ do
      e$ "print" $$ [s "FizzBuzz"]
    elif (n "n" $%$ i 3 $==$ i 0) $ do
      e$ "print" $$ [s "Fizz"]
    elif (n "n" $%$ i 5 $==$ i 0) $ do
      e$ "print" $$ [s "Buzz"]
    else' $ do
      e$ "print" $$ [n "n"]
