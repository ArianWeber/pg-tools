module TypeCheck
  ( checkTypes
  ) where

import           AST
import qualified Data.Map as Map
import qualified Data.Set as Set

checkTypes :: Model -> Either String Env
checkTypes m =
  case buildEnv (graphs m) of
    Left e    -> Left $ show e
    Right env -> ct1 env m
  where
    ct1 :: Env -> Model -> Either String Env
    ct1 env m =
      case ct2 env (graphs m) of
        Right env ->
          case checkHazards env (hazards m) of
            Nothing ->
              case checkSpecs env $ specs m of
                Nothing  -> Right env
                Just err -> Left err
            Just err -> Left err
        err -> err
    ct2 :: Env -> [PG] -> Either String Env
    ct2 env [] = Right env
    ct2 env (pg:pgs) =
      case checkPG env pg of
        Nothing  -> ct2 env pgs
        Just err -> Left err

buildEnv :: [PG] -> Either String Env
buildEnv = be1 emptyEnv
  where
    be1 :: Env -> [PG] -> Either String Env
    be1 acc [] = Right acc
    be1 acc (pg:pgs) =
      case addName acc pg of
        Right acc1 ->
          case addVars acc1 (name pg) (variables pg) of
            Right acc2 -> be1 acc2 pgs
            e          -> e
        e -> e

addName :: Env -> PG -> Either String Env
addName env pg
  | name pg `Map.member` eGraph env = Left $ raiseDef (name pg) $ name pg
  | otherwise =
    case dups (states pg) of
      Nothing ->
        Right $ env {eGraph = Map.insert (name pg) (states pg) (eGraph env)}
      Just s -> Left $ raiseDup (name pg) (name pg) s

addVars :: Env -> String -> [VarDef] -> Either String Env
addVars acc _ [] = Right acc
addVars acc n ((VarDef s r):t)
  | isDefined s acc = Left $ raiseDef n s
  | otherwise =
    case r of
      RBool -> addVars acc {eBool = Set.insert s $ eBool acc} n t
      RInt i j -> addVars acc {eInt = Map.insert s (i, j) $ eInt acc} n t
      REnum xs ->
        case dups xs of
          Nothing -> addVars acc {eEnum = Map.insert s xs (eEnum acc)} n t
          Just e  -> Left $ raiseDup n e s

checkHazards :: Env -> [Hazard] -> Maybe String
checkHazards _ [] = Nothing
checkHazards env (Hazard _ (Left ltl):t) =
  case checkLTL env ltl of
    Nothing -> checkHazards env t
    err     -> err
checkHazards env (Hazard _ (Right ctl):t) =
  case checkCTL env ctl of
    Nothing -> checkHazards env t
    err     -> err

checkSpecs :: Env -> [Spec] -> Maybe String
checkSpecs _ [] = Nothing
checkSpecs env (Spec _ (Left ltl):t) =
  case checkLTL env ltl of
    Nothing -> checkSpecs env t
    err     -> err
checkSpecs env (Spec _ (Right ctl):t) =
  case checkCTL env ctl of
    Nothing -> checkSpecs env t
    err     -> err

checkPG :: Env -> PG -> Maybe String
checkPG env pg =
  case checkF env (name pg) (initialFormula pg) of
    Nothing -> checkTransitions env (name pg) $ transitions pg
    err     -> err

checkTransitions :: Env -> String -> [Trans] -> Maybe String
checkTransitions _ _ [] = Nothing
checkTransitions env n (tr:trs) =
  case checkTrans env n tr of
    Nothing -> checkTransitions env n trs
    err     -> err

checkTrans :: Env -> String -> Trans -> Maybe String
checkTrans env n tr =
  case checkF env n (guard tr) of
    Nothing -> checkAction env n $ action tr
    err     -> err

checkAction :: Env -> String -> Action -> Maybe String
checkAction env n (Action []) = Nothing
checkAction env n (Action (h:t)) =
  case checkAssign env n h of
    Nothing -> checkAction env n $ Action t
    err     -> err

checkAssign :: Env -> String -> Assign -> Maybe String
checkAssign env n (x, Arithmetic t) =
  case checkArithm env n (TermLower x) of
    Nothing -> checkArithm env n t
    err     -> err
checkAssign env n (x, Boolean b) =
  case checkF env n (FVar x) of
    Nothing -> checkF env n b
    err     -> err
checkAssign env n (x, Single s) =
  case eEnum env Map.!? x of
    Just vals ->
      if s `elem` vals
        then Nothing
        else raiseS n s "enum value" x
    Nothing ->
      case checkArithm env n (TermLower x) of
        Nothing -> checkArithm env n $ TermLower s
        err     -> err

checkF :: Env -> String -> Formula -> Maybe String
checkF _ _ FTrue = Nothing
checkF _ _ FFalse = Nothing
checkF env n (FVar s)
  | s `elem` eBool env = Nothing
  | otherwise = Just $ raiseDef n s
checkF env n (Proposition r a b) =
  case a of
    (TermUpper _) -> checkGraphProp env n r a b
    (TermLower s) ->
      if s `Map.member` eEnum env
        then checkEnumProp env n r a b
        else ca a b
    _ -> ca a b
  where
    ca t1 t2 =
      case checkArithm env n a of
        Nothing -> checkArithm env n b
        err     -> err
checkF env n (Not a) = checkF env n a
checkF env n f = cf1 env n f
  where
    cf1 env n (And a b)     = cf2 env n a b
    cf1 env n (Or a b)      = cf2 env n a b
    cf1 env n (Implies a b) = cf2 env n a b
    cf1 env n (Equiv a b)   = cf2 env n a b
    cf2 env n a b =
      case checkF env n a of
        Nothing -> checkF env n b
        err     -> err

checkLTL :: Env -> LTL -> Maybe String
checkLTL _ LTLTrue = Nothing
checkLTL _ LTLFalse = Nothing
checkLTL env (LTLVar s)
  | s `elem` eBool env = Nothing
  | otherwise = Just $ raiseDef "Main module" s
checkLTL env (LTLProp r a b) =
  case a of
    (TermUpper _) -> checkGraphProp env "Main module" r a b
    (TermLower s) ->
      if s `Map.member` eEnum env
        then checkEnumProp env "Main module" r a b
        else ca a b
    _ -> ca a b
  where
    ca t1 t2 =
      case checkArithm env "Main module" a of
        Nothing -> checkArithm env "Main module" b
        err     -> err
checkLTL env (LTLNot a) = checkLTL env a
checkLTL env (LTLX a) = checkLTL env a
checkLTL env (LTLF a) = checkLTL env a
checkLTL env (LTLG a) = checkLTL env a
checkLTL env f = cf1 env f
  where
    cf1 env (LTLAnd a b)     = cf2 env a b
    cf1 env (LTLOr a b)      = cf2 env a b
    cf1 env (LTLImplies a b) = cf2 env a b
    cf1 env (LTLEquiv a b)   = cf2 env a b
    cf1 env (LTLU a b)       = cf2 env a b
    cf2 env a b =
      case checkLTL env a of
        Nothing -> checkLTL env b
        err     -> err

checkCTL :: Env -> CTL -> Maybe String
checkCTL _ CTLTrue = Nothing
checkCTL _ CTLFalse = Nothing
checkCTL env (CTLVar s)
  | s `elem` eBool env = Nothing
  | otherwise = Just $ raiseDef "Main module" s
checkCTL env (CTLProp r a b) =
  case a of
    (TermUpper _) -> checkGraphProp env "Main module" r a b
    (TermLower s) ->
      if s `Map.member` eEnum env
        then checkEnumProp env "Main module" r a b
        else ca a b
    _ -> ca a b
  where
    ca t1 t2 =
      case checkArithm env "Main module" a of
        Nothing -> checkArithm env "Main module" b
        err     -> err
checkCTL env (CTLNot a) = checkCTL env a
checkCTL env (CTLEX a) = checkCTL env a
checkCTL env (CTLEF a) = checkCTL env a
checkCTL env (CTLEG a) = checkCTL env a
checkCTL env (CTLAX a) = checkCTL env a
checkCTL env (CTLAF a) = checkCTL env a
checkCTL env (CTLAG a) = checkCTL env a
checkCTL env f = cf1 env f
  where
    cf1 env (CTLAnd a b)     = cf2 env a b
    cf1 env (CTLOr a b)      = cf2 env a b
    cf1 env (CTLImplies a b) = cf2 env a b
    cf1 env (CTLEquiv a b)   = cf2 env a b
    cf1 env (CTLEU a b)      = cf2 env a b
    cf1 env (CTLAU a b)      = cf2 env a b
    cf2 env a b =
      case checkCTL env a of
        Nothing -> checkCTL env b
        err     -> err

checkGraphProp :: Env -> String -> RelOp -> Term -> Term -> Maybe String
checkGraphProp env n r a b =
  case r of
    Equal -> cgp env a b
    NotEq -> cgp env a b
    _     -> raiseRel n "graph states"
  where
    cgp env (TermUpper s1) t2 =
      case eGraph env Map.!? s1 of
        Just sts ->
          case t2 of
            TermUpper s2 ->
              if s2 `elem` sts
                then Nothing
                else raiseS n s2 "state" s1
            _ -> Just $ n ++ ": " ++ show t2 ++ " is not a state of " ++ s1
        Nothing -> raiseG n s1 "graph"

checkEnumProp :: Env -> String -> RelOp -> Term -> Term -> Maybe String
checkEnumProp env n r a b =
  case r of
    Equal -> cgp env a b
    NotEq -> cgp env a b
    _     -> raiseRel n "enum values"
  where
    cgp env (TermLower s1) t2 =
      case eEnum env Map.!? s1 of
        Just vals ->
          case t2 of
            TermLower s2 ->
              if s2 `elem` vals
                then Nothing
                else raiseS n s2 "enum value" s1
            _ -> Just $ n ++ ": " ++ show t2 ++ " is not a value of " ++ s1
        Nothing -> raiseG n s1 "enum"

checkArithm :: Env -> String -> Term -> Maybe String
checkArithm _ _ (Const _) = Nothing
checkArithm env n (TermUpper s) =
  Just $ n ++ ": " ++ s ++ " not allowed in arithmetic expression"
checkArithm env n (TermLower s)
  | s `Map.member` eInt env = Nothing
  | s `elem` eBool env || s `Map.member` eEnum env = raiseType n s "int"
  | otherwise = raiseUndef n s
checkArithm env n (Negative t) = checkArithm env n t
checkArithm env n t = ct1 env n t
  where
    ct1 env n (Add a b)      = ct2 env n a b
    ct1 env n (Subtract a b) = ct2 env n a b
    ct1 env n (Multiply a b) = ct2 env n a b
    ct1 env n (Divide a b)   = ct2 env n a b
    ct2 env n a b =
      case checkArithm env n a of
        Nothing -> checkArithm env n b
        err     -> err

dups :: (Ord a, Eq a) => [a] -> Maybe a
dups = dups' Set.empty
  where
    dups' _ [] = Nothing
    dups' s (x:xs)
      | x `elem` s = Just x
      | otherwise = dups' (Set.insert x s) xs

isDefined :: String -> Env -> Bool
isDefined s env =
  (s `elem` eBool env) ||
  (s `Map.member` eInt env) || (s `Map.member` eEnum env)

raiseG :: FilePath -> String -> String -> Maybe String
raiseG fp v t = Just $ "File " ++ fp ++ ": " ++ v ++ " is not a defined " ++ t

raiseS :: FilePath -> String -> String -> String -> Maybe String
raiseS fp v s t =
  Just $ fp ++ ": " ++ v ++ " is not a defined " ++ s ++ " of " ++ t

raiseDup :: FilePath -> String -> String -> String
raiseDup fp v s = fp ++ ":  " ++ s ++ " defined twice for " ++ v

raiseDef :: FilePath -> String -> String
raiseDef fp v = fp ++ ":  " ++ v ++ " was already defined somewhere else"

raiseUndef :: FilePath -> String -> Maybe String
raiseUndef fp v = Just $ fp ++ ": tried to reference undefined variable " ++ v

raiseType :: FilePath -> String -> String -> Maybe String
raiseType fp v t = Just $ fp ++ ": " ++ v ++ " is not of type " ++ t

raiseRel :: FilePath -> String -> Maybe String
raiseRel fp s = Just $ fp ++ ": only '=' and '!=' allowed for checking " ++ s
