module Main where

import           Data.List                 (isSuffixOf)
import           Distribution.Simple.Utils (getDirectoryContentsRecursive)
import           Parser                    (parse)
import           Tokenizer                 (tokenize)

main :: IO ()
main = do
  allFiles <- getDirectoryContentsRecursive "."
  let pgFiles = filter (".pg" `isSuffixOf`) allFiles
  contents <- mapM readFile pgFiles
  let graphs = map (parse . tokenize) contents
  print graphs
