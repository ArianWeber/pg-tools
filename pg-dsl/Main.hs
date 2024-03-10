module Main where

import           Parser    (parse)
import           Tokenizer (tokenize)

main :: IO ()
main = do
  input <- readFile "./robot.txt"
  let tokens = tokenize input
      ast = parse tokens
  print ast
