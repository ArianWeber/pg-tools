module Main where

import           Tokenizer (tokenize)

main :: IO ()
main = do
  input <- readFile "./robot.txt"
  let tokens = tokenize input
  print tokens
