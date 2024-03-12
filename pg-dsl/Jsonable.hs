module Jsonable
  ( Jsonable(toJson)
  ) where

import           AST
import           Distribution.Utils.Json (Json (..), (.=))

class Jsonable a where
  toJson :: a -> Json

instance Jsonable String where
  toJson :: String -> Json
  toJson = JsonString

instance Jsonable AST where
  toJson :: AST -> Json
  toJson (Left e)  = JsonObject ["error" .= JsonString (show e)]
  toJson (Right m) = toJson m

instance Jsonable Model where
  toJson :: Model -> Json
  toJson m = JsonObject [modelName m .= JsonArray (map toJson $ graphs m)]

instance Jsonable PG where
  toJson :: PG -> Json
  toJson g =
    JsonObject
      [ name g .=
        JsonObject
          [ "states" .= JsonArray (map toJson (states g))
          , "variables" .= JsonArray (map toJson (variables g))
          , "transitions" .= JsonArray (map toJson (transitions g))
          ]
      ]

instance Jsonable VarDef where
  toJson :: VarDef -> Json
  toJson (VarDef s r) =
    case r of
      RBool    -> mkObj s $ JsonArray [JsonBool True, JsonBool False]
      RInt i j -> mkObj s $ JsonString (show i ++ ".." ++ show j)
      REnum e  -> mkObj s $ JsonArray $ map JsonString e

mkObj :: String -> Json -> Json
mkObj s r = JsonObject [s .= JsonObject ["range" .= r, "init" .= JsonNull]]

instance Jsonable Trans where
  toJson :: Trans -> Json
  toJson t =
    JsonObject
      [ (preState t ++ " -> " ++ postState t) .=
        JsonObject
          [ "precon" .= JsonNull
          , "guard" .= toJson (guard t)
          , "action" .= toJson (action t)
          ]
      ]

instance Jsonable Action where
  toJson :: Action -> Json
  toJson a =
    let s = show a
     in if s == ""
          then JsonNull
          else JsonString s

instance Jsonable Formula where
  toJson :: Formula -> Json
  toJson f =
    case f of
      FTrue -> JsonNull
      FFalse -> JsonBool False
      _ ->
        let s = show f
         in if s == "true"
              then JsonNull
              else JsonString s
