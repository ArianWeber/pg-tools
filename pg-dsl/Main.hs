module Main where

import           AST                       (AST, FParserError (..), Model (..),
                                            PG, ParserError, ProgramGraph)
import qualified Data.ByteString.Lazy      as LBS
import           Data.List                 (isSuffixOf)
import           Distribution.Simple.Utils (getDirectoryContentsRecursive)
import           Distribution.Utils.Json   (renderJson)
import           GHC.Base                  (returnIO)
import           Jsonable                  (Jsonable (toJson))
import           Parser                    (parse, parseMain)
import           Tokenizer                 (tokenize)

main :: IO ()
main = do
  let prefix = "./examples/robot/"
  allFiles <- getDirectoryContentsRecursive prefix
  let pgFiles = filter isGraphFile allFiles
  contents <- mapM (readFile . (prefix ++)) pgFiles
  let graphs = inspect $ zip pgFiles $ map (parse . tokenize) contents
      mainFile = prefix ++ "main.pg"
  mainContent <- readFile mainFile
  let modelRaw = inspectMain mainFile $ (parseMain . tokenize) mainContent
      model = mergeGraphs modelRaw graphs
  LBS.writeFile "./output.json" $ renderJson $ toJson model
  print $ tokenize mainContent
  print model

isGraphFile :: String -> Bool
isGraphFile s = ".pg" `isSuffixOf` s && not ("main.pg" `isSuffixOf` s)

readGraph :: FilePath -> IO (String, String)
readGraph fp = do
  content <- readFile fp
  returnIO (content, fp)

inspect :: [(FilePath, ProgramGraph)] -> Either FParserError [PG]
inspect = mg []
  where
    mg acc []                = Right acc
    mg acc ((fp, Left e):t)  = Left $ FPError fp e
    mg acc ((_, Right pg):t) = mg (pg : acc) t

inspectMain :: FilePath -> Either ParserError Model -> Either FParserError Model
inspectMain fp (Left err) = Left $ FPError fp err
inspectMain _ (Right m)   = Right m

mergeGraphs :: AST -> Either FParserError [PG] -> AST
mergeGraphs (Left e) _ = Left e
mergeGraphs _ (Left e) = Left e
mergeGraphs (Right m) (Right g) =
  Right $
  Model
    { modelName = modelName m
    , graphs = graphs m ++ g
    , hazards = hazards m
    , specs = specs m
    }
