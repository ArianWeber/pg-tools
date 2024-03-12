module Main where

import           AST                       (AST, Model (..), PG, ParserError,
                                            ProgramGraph)
import qualified Data.ByteString.Lazy      as LBS
import           Data.List                 (isSuffixOf)
import           Distribution.Simple.Utils (getDirectoryContentsRecursive)
import           Distribution.Utils.Json   (renderJson)
import           Jsonable                  (Jsonable (toJson))
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
  let modelRaw = parseMain $ tokenize mainContent
      model = mergeGraphs modelRaw regularGraphs
  LBS.writeFile "./output.json" $ renderJson $ toJson model
  print model

isGraphFile :: String -> Bool
isGraphFile s = ".pg" `isSuffixOf` s && not ("main.pg" `isSuffixOf` s)

inspectGraphs :: [ProgramGraph] -> Either ParserError [PG]
inspectGraphs = mg []
  where
    mg :: [PG] -> [ProgramGraph] -> Either ParserError [PG]
    mg acc []           = Right acc
    mg acc (Left e:t)   = Left e
    mg acc (Right pg:t) = mg (pg : acc) t

mergeGraphs :: AST -> Either ParserError [PG] -> AST
mergeGraphs (Left e) _ = Left e
mergeGraphs _ (Left e) = Left e
mergeGraphs (Right m) (Right g) =
  Right $ Model {modelName = modelName m, graphs = graphs m ++ g}
