module Jsonable
  ( Jsonable(toJson)
  ) where

import           AST
import qualified Data.Map                as Map
import           Distribution.Utils.Json (Json (..), (.=))

class Jsonable a where
  toJson :: a -> Json

instance Jsonable String where
  toJson :: String -> Json
  toJson = JsonString

instance Jsonable AST where
  toJson :: AST -> Json
  toJson (Left e)  = JsonObject ["error" .= JsonString e]
  toJson (Right m) = toJson m

instance Jsonable Model where
  toJson :: Model -> Json
  toJson m =
    JsonObject
      [ modelName m .=
        JsonObject
          [ "components" .= JsonArray (map (pgJson $ environ m) $ graphs m)
          , "hazards" .= JsonArray (map toJson $ hazards m)
          , "specification" .= JsonArray (map toJson $ specs m)
          ]
      ]

pgJson :: Env -> PG -> Json
pgJson env g =
  JsonObject
    [ name g .=
      JsonObject
        [ "states" .= JsonArray (map toJson (states g))
        , "variables" .= JsonArray (map toJson (variables g))
        , "init" .=
          JsonObject
            ["string" .= JsonString (jsonInit g), "type" .= JsonString "pl"]
        , "transitions" .= JsonArray (map (transJson env) (transitions g))
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

transJson :: Env -> Trans -> Json
transJson env t =
  JsonObject
    [ (preState t ++ " -> " ++ postState t) .=
      JsonObject
        [ "precon" .= computePrecon env (action t)
        , "guard" .=
          JsonObject ["string" .= toJson (guard t), "type" .= JsonString "pl"]
        , "action" .= toJson (action t)
        ]
    ]

computePrecon :: Env -> Action -> Json
computePrecon _ (Action []) = JsonNull
computePrecon env act = cp "" env act
  where
    cp acc env (Action []) =
      JsonObject ["string" .= JsonString acc, "type" .= JsonString "pl"]
    cp acc env (Action (h:t)) = cp (compPrecon env h ++ " && ") env (Action t)

compPrecon :: Env -> Assign -> String
compPrecon env (v, t) =
  let (i, j) = eInt env Map.! v
   in show t ++ " >= " ++ show i ++ " && " ++ show t ++ " <= " ++ show j

instance Jsonable Action where
  toJson :: Action -> Json
  toJson a =
    let s = show a
     in if s == ""
          then JsonNull
          else JsonObject
                 ["string" .= JsonString s, "type" .= JsonString "action"]

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

jsonInit :: PG -> String
jsonInit pg
  | initialFormula pg == FTrue = show initState
  | otherwise = show $ And initState $ initialFormula pg
  where
    initState =
      Proposition Equal (TermUpper $ name pg) (TermUpper $ initialState pg)
