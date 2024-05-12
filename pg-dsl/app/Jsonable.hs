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
    let m' = replaceNofaults m
     in JsonObject
          [ modelName m' .=
            JsonObject
              [ "components" .=
                JsonArray (map (pgJson $ environ m') $ graphs m')
              , "hazards" .= JsonArray (map toJson $ hazards m')
              , "specification" .= JsonArray (map toJson $ specs m')
              ]
          ]

replaceNofaults :: Model -> Model
replaceNofaults m =
  m
    { hazards = map (replaceHazard (graphs m)) $ hazards m
    , specs = map (replaceSpec (graphs m)) $ specs m
    }

replaceHazard :: [PG] -> Hazard -> Hazard
replaceHazard pgs (Hazard s (Left ltl))  = Hazard s (Left $ replLTL pgs ltl)
replaceHazard pgs (Hazard s (Right ctl)) = Hazard s (Right $ replCTL pgs ctl)

replaceSpec :: [PG] -> Spec -> Spec
replaceSpec pgs (Spec s (Left ltl))  = Spec s (Left $ replLTL pgs ltl)
replaceSpec pgs (Spec s (Right ctl)) = Spec s (Right $ replCTL pgs ctl)

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
        , "represents_fault" .= JsonBool (isFault g)
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
            JsonObject ["string" .= JsonString f, "type" .= JsonString "tl"]
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
        , "guard" .= guardJson (guard t)
        , "action" .= toJson (action t)
        ]
    ]

guardJson :: Formula -> Json
guardJson f =
  case toJson f of
    JsonNull -> JsonNull
    x        -> JsonObject ["string" .= x, "type" .= JsonString "guard"]

computePrecon :: Env -> Action -> Json
computePrecon _ (Action []) = JsonNull
computePrecon env act = cp "" env act
  where
    cp acc env (Action []) =
      JsonObject ["string" .= JsonString acc, "type" .= JsonString "guard"]
    cp acc env (Action [h]) = cp (compPrecon env h) env (Action [])
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
  toJson CTLTrue             = JsonNull
  toJson CTLFalse            = JsonBool False
  toJson (CTLVar "nofaults") = undefined
  toJson f                   = JsonString $ show f

jsonInit :: PG -> String
jsonInit pg
  | initialFormula pg == FTrue = show initState
  | otherwise = show $ And initState $ initialFormula pg
  where
    initState =
      Proposition Equal (TermUpper $ name pg) (TermUpper $ initialState pg)

nofaults :: [PG] -> Formula
nofaults pgs =
  agg $
  map
    ((\x -> Proposition Equal (TermUpper x) (TermUpper "No")) . name)
    (filter isFault pgs)
  where
    agg [] = FTrue
    agg x  = foldr And (head x) (tail x)

nofaultsLTL :: [PG] -> LTL
nofaultsLTL pgs =
  agg $
  map
    ((\x -> LTLProp Equal (TermUpper x) (TermUpper "No")) . name)
    (filter isFault pgs)
  where
    agg [] = LTLTrue
    agg x  = foldr LTLAnd (head x) (tail x)

nofaultsCTL :: [PG] -> CTL
nofaultsCTL pgs =
  agg $
  map
    ((\x -> CTLProp Equal (TermUpper x) (TermUpper "No")) . name)
    (filter isFault pgs)
  where
    agg [] = CTLTrue
    agg x  = foldr CTLAnd (head x) (tail x)

replF :: [PG] -> Formula -> Formula
replF pgs (FVar "nofaults") = nofaults pgs
replF pgs (Not f)           = Not (replF pgs f)
replF pgs (And f g)         = And (replF pgs f) (replF pgs g)
replF pgs (Or f g)          = Or (replF pgs f) (replF pgs g)
replF pgs (Implies f g)     = Implies (replF pgs f) (replF pgs g)
replF pgs (Equiv f g)       = Equiv (replF pgs f) (replF pgs g)

replLTL :: [PG] -> LTL -> LTL
replLTL pgs (LTLVar "nofaults") = nofaultsLTL pgs
replLTL pgs (LTLNot f)          = LTLNot (replLTL pgs f)
replLTL pgs (LTLX f)            = LTLX (replLTL pgs f)
replLTL pgs (LTLF f)            = LTLF (replLTL pgs f)
replLTL pgs (LTLG f)            = LTLG (replLTL pgs f)
replLTL pgs (LTLAnd f g)        = LTLAnd (replLTL pgs f) (replLTL pgs g)
replLTL pgs (LTLOr f g)         = LTLOr (replLTL pgs f) (replLTL pgs g)
replLTL pgs (LTLImplies f g)    = LTLImplies (replLTL pgs f) (replLTL pgs g)
replLTL pgs (LTLEquiv f g)      = LTLEquiv (replLTL pgs f) (replLTL pgs g)
replLTL pgs (LTLU f g)          = LTLU (replLTL pgs f) (replLTL pgs g)
replLTL _ x                     = x

replCTL :: [PG] -> CTL -> CTL
replCTL pgs (CTLVar "nofaults") = nofaultsCTL pgs
replCTL pgs (CTLNot f)          = CTLNot (replCTL pgs f)
replCTL pgs (CTLEX f)           = CTLEX (replCTL pgs f)
replCTL pgs (CTLEF f)           = CTLEF (replCTL pgs f)
replCTL pgs (CTLEG f)           = CTLEG (replCTL pgs f)
replCTL pgs (CTLAX f)           = CTLAX (replCTL pgs f)
replCTL pgs (CTLAF f)           = CTLAF (replCTL pgs f)
replCTL pgs (CTLAG f)           = CTLAG (replCTL pgs f)
replCTL pgs (CTLAnd f g)        = CTLAnd (replCTL pgs f) (replCTL pgs g)
replCTL pgs (CTLOr f g)         = CTLOr (replCTL pgs f) (replCTL pgs g)
replCTL pgs (CTLImplies f g)    = CTLImplies (replCTL pgs f) (replCTL pgs g)
replCTL pgs (CTLEquiv f g)      = CTLEquiv (replCTL pgs f) (replCTL pgs g)
replCTL pgs (CTLEU f g)         = CTLEU (replCTL pgs f) (replCTL pgs g)
replCTL pgs (CTLAU f g)         = CTLAU (replCTL pgs f) (replCTL pgs g)
replCTL _ x                     = x
