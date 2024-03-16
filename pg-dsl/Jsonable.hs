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
  toJson m =
    JsonObject
      [ modelName m .=
        JsonObject
          [ "components" .= JsonArray (map toJson $ graphs m)
          , "hazards" .= JsonArray (map toJson $ hazards m)
          , "specification" .= JsonArray (map toJson $ specs m)
          ]
      ]

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

instance Jsonable Hazard where
  toJson :: Hazard -> Json
  toJson (Hazard s h) =
    let f =
          case h of
            Left ltl  -> show ltl
            Right ctl -> show ctl
     in JsonObject ["label" .= JsonString s, "expression" .= JsonString f]

instance Jsonable Spec where
  toJson :: Spec -> Json
  toJson (Spec s h) =
    let f =
          case h of
            Left ltl  -> show ltl
            Right ctl -> show ctl
     in JsonObject
          [ "label" .= JsonString s
          , "expression" .=
            JsonObject ["string" .= JsonString f, "type" .= JsonString "pl"]
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
  toJson FTrue  = JsonNull
  toJson FFalse = JsonBool False
  toJson f      = JsonString $ show f

instance Jsonable LTL where
  toJson :: LTL -> Json
  toJson LTLTrue  = JsonNull
  toJson LTLFalse = JsonBool False
  toJson f        = JsonString $ show f

instance Jsonable CTL where
  toJson :: CTL -> Json
  toJson CTLTrue  = JsonNull
  toJson CTLFalse = JsonBool False
  toJson f        = JsonString $ show f