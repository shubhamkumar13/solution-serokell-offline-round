module Main where

import Runner
import Todo

main :: IO ()
main = do
  n <- (read :: String -> Int) <$> getLine
  runTodoList $ runNQueries n
