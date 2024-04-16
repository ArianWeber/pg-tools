module Main where

import           AST                       (AST, FParserError (..), Model (..),
                                            PG (..), ParserError, ProgramGraph,
                                            emptyEnv)
import           Control.Monad             (when)
import qualified Data.ByteString.Lazy      as LBS
import           Data.Char                 (toLower)
import           Data.List                 (isSuffixOf)
import           Distribution.Simple.Utils (getDirectoryContentsRecursive)
import           Distribution.Utils.Json   (renderJson)
import           GHC.Base                  (returnIO)
import           Jsonable                  (Jsonable (toJson))
import           Parser                    (parse, parseMain)
import           RangeCheck                (checkRanges)
import           System.Directory          (createDirectoryIfMissing)
import           System.Environment        (getArgs)
import           System.Exit               (ExitCode (ExitFailure), exitSuccess,
                                            exitWith)
import           System.FilePath           (takeDirectory)
import           System.Process            (createProcess, proc, waitForProcess)
import           Tokenizer                 (tokenize)
import           TypeCheck                 (checkTypes)

main :: IO ()
main = do
  args <- getArgs
  let (fp, cmd) = parseArgs args
      basepath =
        if fp /= "" && last fp == '/'
          then fp
          else fp ++ "/"
  when (cmd == ["err"]) $
    putStrLn ("Invalid command line arguments: " ++ unwords args) >>
    exitWith (ExitFailure 1)
  allFiles <- getDirectoryContentsRecursive basepath
  let pgFiles = filter isGraphFile allFiles
  contents <- mapM (readFile . (basepath ++)) pgFiles
  let inspected = inspect $ zip pgFiles $ map (parse . tokenize) contents
      mainFile = basepath ++ "main.pg"
  mainContent <- readFile mainFile
  let modelRaw = inspectMain mainFile $ (parseMain . tokenize) mainContent
      model = mergeGraphs modelRaw inspected
      checkedModel =
        case model of
          Right m ->
            case checkTypes m of
              Right env ->
                case checkRanges env m of
                  Nothing  -> Right m {environ = env}
                  Just err -> Left err
              Left s -> Left s
          err -> err
  when (isError checkedModel) $
    putStrLn (getErrorMsg checkedModel) >> exitWith (ExitFailure 1)
  let jsonOut = "./out/pg-dsl/" ++ map toLower (getModelName model) ++ ".json"
      jsonArgs = ["--json-file", jsonOut]
  createDirectoryIfMissing True $ takeDirectory jsonOut
  LBS.writeFile jsonOut $ renderJson $ toJson checkedModel
  when (cmd /= []) $ do
    (_, _, _, processHandle) <-
      createProcess (proc "pg-verify" $ cmd ++ jsonArgs)
    _ <- waitForProcess processHandle
    exitSuccess
  exitSuccess

parseArgs :: [String] -> (FilePath, [String])
parseArgs [] = (".", [])
parseArgs [x]
  | x `elem` ["-t", "--test"] = (".", ["test"])
  | x `elem` ["-s", "--simulate"] = (".", ["simulate"])
  | x `elem` ["-d", "--dcca"] = (".", ["dcca"])
  | notDash x = (x, [])
parseArgs [x, y]
  | x `elem` ["-t", "--test"] && notDash y = (y, ["test"])
  | x `elem` ["-s", "--simulate"] && notDash y = (y, ["simulate"])
  | x `elem` ["-d", "--dcca"] && notDash y = (y, ["dcca"])
  | x `elem` ["-o", "--show"] && y `elem` ["puml", "json", "yaml"] =
    (".", ["show", y])
  | x `elem` ["-o", "--show"] && y == "png" =
    (".", ["show", y, "--hide-precons"])
parseArgs [x, y, z]
  | x `elem` ["-o", "--show"] && y `elem` ["puml", "json", "yaml"] && notDash z =
    (z, ["show", y])
  | x `elem` ["-o", "--show"] && y == "png" = (z, ["show", y, "--hide-precons"])
parseArgs x = ("", ["err"])

notDash :: String -> Bool
notDash [] = False
notDash s  = head s /= '-'

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

mergeGraphs :: Either FParserError Model -> Either FParserError [PG] -> AST
mergeGraphs (Left e) _ = Left $ show e
mergeGraphs _ (Left e) = Left $ show e
mergeGraphs (Right m) (Right g) =
  Right $
  Model
    { modelName = modelName m
    , graphs = graphs m ++ g
    , hazards = hazards m
    , specs = specs m
    , environ = emptyEnv
    }

getModelName :: AST -> String
getModelName (Right m) = modelName m

isError :: AST -> Bool
isError (Left _) = True
isError _        = False

getErrorMsg :: AST -> String
getErrorMsg (Left s) = s
