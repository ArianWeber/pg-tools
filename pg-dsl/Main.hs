module Main where

import           AST                       (PError, PG, ProgramGraph)
import           Data.List                 (isSuffixOf)
import           Distribution.Simple.Utils (getDirectoryContentsRecursive)
import           Parser                    (parse, parseMain)
import           Tokenizer                 (tokenize)

main :: IO ()
main = do
  let prefix = "./examples/robot/"
  allFiles <- getDirectoryContentsRecursive prefix
  let pgFiles = filter isGraphFile allFiles
  contents <- mapM (readFile . (prefix ++)) pgFiles
  let regularGraphs = inspectGraphs $ map (parse . tokenize) contents
      mainFile = prefix ++ "main.pg"
  mainContent <- readFile mainFile
  let errorGraphs = parseMain $ tokenize mainContent
      graphs = mergeGraphs regularGraphs errorGraphs
  print graphs

isGraphFile :: String -> Bool
isGraphFile s = ".pg" `isSuffixOf` s && not ("main.pg" `isSuffixOf` s)

inspectGraphs :: [ProgramGraph] -> Either PError [PG]
inspectGraphs = mg []
  where
    mg :: [PG] -> [ProgramGraph] -> Either PError [PG]
    mg acc []           = Right acc
    mg acc (Left e:t)   = Left e
    mg acc (Right pg:t) = mg (pg : acc) t

mergeGraphs :: Either PError [PG] -> Either PError [PG] -> Either PError [PG]
mergeGraphs (Left e) _            = Left e
mergeGraphs _ (Left e)            = Left e
mergeGraphs (Right g1) (Right g2) = Right (g1 ++ g2)
