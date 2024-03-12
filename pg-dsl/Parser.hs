module Parser
  ( parse
  , parseMain
  ) where

import           AST   (Action, Expression (..), Formula (..), PError (..),
                        PG (..), ProgramGraph, Range (..), RelOp (..), State,
                        Term (..), Trans (..), VarDef)
import           Token (DToken (..), TError (..), Token (..), TokenList)

parseMain :: TokenList -> Either PError [PG]
parseMain (Left e) =
  Left $ PError {pMsg = tMsg e, pLine = tLine e, pCol = tCol e}
parseMain (Right t) =
  case pErrors (filter (not . ignoreToken) t) of
    Left e -> Left e
    Right (pg, h:_) ->
      case token h of
        TEoF -> Right pg
        _    -> raise h

parse :: TokenList -> ProgramGraph
parse (Left e) = Left $ PError {pMsg = tMsg e, pLine = tLine e, pCol = tCol e}
parse (Right t) =
  case pGraph (filter (not . ignoreToken) t) of
    Left e -> Left e
    Right (pg, h:_) ->
      case token h of
        TEoF -> Right pg
        _    -> raise h

pErrors :: [DToken] -> Either PError ([PG], [DToken])
pErrors p@(h:t) =
  case token h of
    TEoF    -> Right ([], p)
    TErrors -> pe1 t
    _       -> raise h
  where
    pe1 :: [DToken] -> Either PError ([PG], [DToken])
    pe1 (h:t) =
      case token h of
        TCurlyL -> pe2 [] t
        _       -> raise h
    pe2 :: [PG] -> [DToken] -> Either PError ([PG], [DToken])
    pe2 acc (h:t) =
      case token h of
        TCurlyR     -> Right (acc, t)
        TTransient  -> pe3 False acc t
        TPersistent -> pe3 True acc t
        _           -> raise h
    pe3 :: Bool -> [PG] -> [DToken] -> Either PError ([PG], [DToken])
    pe3 persistent acc (h:t) =
      case token h of
        TUpper s ->
          if persistent
            then pe2 (mkPersistent s : acc) t
            else pe2 (mkTransient s : acc) t
        _ -> raise h

pGraph :: [DToken] -> Either PError (PG, [DToken])
pGraph (h:t) =
  case token h of
    TGraph -> pg1 t
    _      -> raise h
  where
    pg1 (h:t) =
      case token h of
        TUpper s -> pg2 s t
        _        -> raise h
    pg2 n (h:t) =
      case token h of
        TCurlyL -> pg3 n t
        _       -> raise h
    pg3 n p =
      case pVars p of
        Right (v, p') -> pg4 v n p'
        Left e        -> Left e
    pg4 v n p =
      case pStates p of
        Right (s, p') -> pg5 s v n p'
        Left e        -> Left e
    pg5 s v n p =
      case pInit p of
        Right ((iS, iF), p') -> pg6 (iS, iF) s v n p'
        Left e               -> Left e
    pg6 (iS, iF) s v n p =
      case pTransitions p of
        Right (t, p') -> pg7 t (iS, iF) s v n p'
        Left e        -> Left e
    pg7 tr (iS, iF) s v n (h:t) =
      case token h of
        TCurlyR ->
          Right
            ( PG
                { name = n
                , variables = v
                , states = s
                , transitions = tr
                , initialState = iS
                , initialFormula = iF
                , isFault = False
                }
            , t)
        _ -> raise h

pVars :: [DToken] -> Either PError ([VarDef], [DToken])
pVars p@(h:t) =
  case token h of
    TVars   -> pv1 t
    TStates -> Right ([], p)
    _       -> raise h
  where
    pv1 (h:t) =
      case token h of
        TCurlyL -> pv2 [] t
        _       -> raise h
    pv2 vs (h:t) =
      case token h of
        TBool ->
          case pBool t of
            Right (v, p) -> pv2 (v : vs) p
            Left e       -> Left e
        TInt ->
          case pInt t of
            Right (v, p) -> pv2 (v : vs) p
            Left e       -> Left e
        TEnum ->
          case pEnum t of
            Right (v, p) -> pv2 (v : vs) p
            Left e       -> Left e
        TState ->
          case pState t of
            Right (v, p) -> pv2 (v : vs) p
            Left e       -> Left e
        TCurlyR -> Right (vs, t)
        _ -> raise h

pBool :: [DToken] -> Either PError (VarDef, [DToken])
pBool (h:t) =
  case token h of
    TLower s -> Right ((s, RBool), t)
    _        -> raise h

pInt :: [DToken] -> Either PError (VarDef, [DToken])
pInt (h:t) =
  case token h of
    TLower s -> pi1 s t
    _        -> raise h
  where
    pi1 s (h:t) =
      case token h of
        TSquareL -> pi2 s t
        _        -> raise h
    pi2 s (h:t) =
      case token h of
        TNumber i -> pi3 i s t
        _         -> raise h
    pi3 i s (h:t) =
      case token h of
        TDots -> pi4 i s t
        _     -> raise h
    pi4 i s (h:t) =
      case token h of
        TNumber j -> pi5 i j s t
        _         -> raise h
    pi5 i j s (h:t) =
      case token h of
        TSquareR -> Right ((s, RInt i j), t)
        _        -> raise h

pEnum :: [DToken] -> Either PError (VarDef, [DToken])
pEnum (h:t) =
  case token h of
    TLower s -> pe1 s t
    _        -> raise h
  where
    pe1 s (h:t) =
      case token h of
        TCurlyL -> pe2 [] s t
        _       -> raise h
    pe2 vs s (h:t) =
      case token h of
        TLower v -> pe3 (v : vs) s t
        _        -> raise h
    pe3 vs s (h:t) =
      case token h of
        TComma  -> pe2 vs s t
        TCurlyR -> Right ((s, REnum vs), t)
        _       -> raise h

pState :: [DToken] -> Either PError (VarDef, [DToken])
pState (h:t) =
  case token h of
    TUpper s -> Right ((s, RState), t)
    _        -> raise h

pStates :: [DToken] -> Either PError ([State], [DToken])
pStates (h:t) =
  case token h of
    TStates -> ps1 t
    _       -> raise h
  where
    ps1 (h:t) =
      case token h of
        TCurlyL -> ps2 [] t
        _       -> raise h
    ps2 ss (h:t) =
      case token h of
        TUpper s -> ps3 (s : ss) t
        _        -> raise h
    ps3 ss (h:t) =
      case token h of
        TComma  -> ps2 ss t
        TCurlyR -> Right (ss, t)
        _       -> raise h

pInit :: [DToken] -> Either PError ((State, Formula), [DToken])
pInit (h:t) =
  case token h of
    TInit -> pi1 t
    _     -> raise h
  where
    pi1 (h:t) =
      case token h of
        TColon -> pi2 t
        _      -> raise h
    pi2 (h:t) =
      case token h of
        TUpper s -> pi3 s t
        _        -> raise h
    pi3 s (h:t) =
      case token h of
        TCurlyL -> pi4 s t
        _       -> raise h
    pi4 s p@(h:t) =
      case token h of
        TCurlyR -> Right ((s, FTrue), t)
        _       -> pi5 s p
    pi5 s p =
      case pFormula p of
        Right (f, p') -> pi6 f s p'
        Left e        -> Left e
    pi6 f s (h:t) =
      case token h of
        TCurlyR -> Right ((s, f), t)
        _       -> raise h

pTransitions :: [DToken] -> Either PError ([Trans], [DToken])
pTransitions (h:t) =
  case token h of
    TTransitions -> pt1 t
    _            -> raise h
  where
    pt1 (h:t) =
      case token h of
        TCurlyL -> pt2 [] t
        _       -> raise h
    pt2 ts p@(h:t) =
      case token h of
        TUpper _ -> pt3 ts p
        TCurlyR  -> Right (ts, t)
        _        -> raise h
    pt3 ts p =
      case pTrans p of
        Right (t, p') -> pt2 (t : ts) p'
        Left e        -> Left e

pTrans :: [DToken] -> Either PError (Trans, [DToken])
pTrans (h:t) =
  case token h of
    TUpper s -> pt1 s t
    _        -> raise h
  where
    pt1 s (h:t) =
      case token h of
        TArrow -> pt2 s t
        _      -> raise h
    pt2 s (h:t) =
      case token h of
        TUpper s' -> pt3 s' s t
        _         -> raise h
    pt3 s' s (h:t) =
      case token h of
        TCurlyL -> pt4 s' s t
        _       -> raise h
    pt4 s' s p@(h:t) =
      case token h of
        TGuard  -> pt5 s' s t
        TAction -> pt8 FTrue s' s p
        TCurlyR -> Right (mkTrans s FTrue [] s', t)
        _       -> raise h
    pt5 s' s (h:t) =
      case token h of
        TCurlyL -> pt6 s' s t
        _       -> raise h
    pt6 s' s p =
      case pFormula p of
        Right (f, p') -> pt7 f s' s p'
        Left e        -> Left e
    pt7 f s' s (h:t) =
      case token h of
        TCurlyR -> pt8 f s' s t
        _       -> raise h
    pt8 f s' s p =
      case pAction p of
        Right (a, p') -> pt9 a f s' s p'
        Left e        -> Left e
    pt9 a f s' s (h:t) =
      case token h of
        TCurlyR -> Right (mkTrans s f a s', t)
        _       -> raise h

pAction :: [DToken] -> Either PError (Action, [DToken])
pAction p@(h:t) =
  case token h of
    TAction -> pa1 t
    TCurlyR -> Right ([], p)
    _       -> raise h
  where
    pa1 (h:t) =
      case token h of
        TCurlyL -> pa2 [] t
        _       -> raise h
    pa2 acc (h:t) =
      case token h of
        TLower s -> pa3 s acc t
        TCurlyR  -> Right (acc, t)
        _        -> raise h
    pa3 v acc (h:t) =
      case token h of
        TWalrus -> pa4 v acc t
        _       -> raise h
    pa4 v acc p =
      case pExpression p of
        Right (e, p') -> pa5 ((v, e) : acc) p'
        Left e        -> Left e
    pa5 acc (h:t) =
      case token h of
        TCurlyR -> Right (acc, t)
        TSemic  -> pa2 acc t
        _       -> raise h

pExpression :: [DToken] -> Either PError (Expression, [DToken])
pExpression p =
  case pSingle p of
    Right (e, p') -> Right (Single e, p')
    _ ->
      case pTerm p of
        Right (e, p') -> Right (Arithmetic e, p')
        _ ->
          case pFormula p of
            Right (e, p') -> Right (Boolean e, p')
            Left e        -> Left e

pSingle :: [DToken] -> Either PError (String, [DToken])
pSingle (h:t) =
  case token h of
    TLower s -> ps1 s t
    _        -> raise h
  where
    ps1 s p@(h:_) =
      case token h of
        TCurlyR -> Right (s, p)
        TSemic  -> Right (s, p)
        _       -> raise h

pFormula :: [DToken] -> Either PError (Formula, [DToken])
pFormula p@(h:t) =
  case token h of
    TBRacketL -> pf1 [] [] True t
    _         -> pf1 [] [] False p
  where
    pf1 fs ops b p@(h:t) =
      case token h of
        TBRacketL ->
          case pf1 fs ops b t of
            Right (form, p') -> pf3 (form : fs) ops True t
            Left e           -> Left e
        TTrue -> pf3 (FTrue : fs) ops b t
        TFalse -> pf3 (FFalse : fs) ops b t
        TLower s ->
          case pProposition p of
            Right (f, p') -> pf3 (f : fs) ops b p'
            Left _        -> pf3 (FVar s : fs) ops b t
        TUpper s ->
          case pProposition p of
            Right (f, p') -> pf3 (f : fs) ops b p'
            Left e        -> Left e
        TNot -> pf2 fs ops b t
        _ -> raise h
    pf2 fs ops b (h:t) =
      case token h of
        TBRacketL ->
          case pf1 fs ops b t of
            Right (form, p') -> pf3 (Not form : fs) ops True t
            Left e           -> Left e
        TTrue -> pf3 (Not FTrue : fs) ops b t
        TFalse -> pf3 (Not FFalse : fs) ops b t
        TLower s ->
          case pProposition p of
            Right (f, p') -> pf3 (Not f : fs) ops b p'
            Left _        -> pf3 (Not (FVar s) : fs) ops b t
        TUpper s ->
          case pProposition p of
            Right (f, p') -> pf3 (Not f : fs) ops b p'
            Left e        -> Left e
        _ -> raise h
    pf3 fs ops b p@(h:t) =
      case token h of
        TAnd -> pf1 fs (TAnd : ops) b t
        TOr -> pf1 fs (TOr : ops) b t
        TImplies -> pf1 fs (TImplies : ops) b t
        TEquiv -> pf1 fs (TEquiv : ops) b t
        TBracketR ->
          if b
            then Right (mergeFormula fs ops, t)
            else raise h
        TCurlyR ->
          if b
            then raise h
            else Right (mergeFormula fs ops, p)
        TSemic ->
          if b
            then raise h
            else Right (mergeFormula fs ops, p)
        _ -> raise h

pProposition :: [DToken] -> Either PError (Formula, [DToken])
pProposition p =
  case pTerm p of
    Right (t, p') -> pp1 t p'
    Left e        -> Left e
  where
    pp1 t1 (h:t) =
      case token h of
        TEq      -> pp2 Equal t1 t
        TNEq     -> pp2 NotEq t1 t
        TLess    -> pp2 Less t1 t
        TLEq     -> pp2 LessEq t1 t
        TGreater -> pp2 Greater t1 t
        TGEq     -> pp2 GreaterEq t1 t
    pp2 op t1 p =
      case pTerm p of
        Right (t2, p') -> Right (Proposition op t1 t2, p')
        Left e         -> Left e

pTerm :: [DToken] -> Either PError (Term, [DToken])
pTerm p@(h:t) =
  case token h of
    TUpper s  -> Right (TermUpper s, t)
    TBRacketL -> pt1 [] [] True t
    _         -> pt1 [] [] False p
  where
    pt1 terms ops b (h:t) =
      case token h of
        TBRacketL ->
          case pt1 terms ops b t of
            Right (term, p') -> pt3 (term : terms) ops True t
            Left e           -> Left e
        TNumber n -> pt3 (Const n : terms) ops b t
        TLower s -> pt3 (TermLower s : terms) ops b t
        TMinus -> pt2 terms ops b t
        _ -> raise h
    pt2 terms ops b (h:t) =
      case token h of
        TBRacketL ->
          case pt1 terms ops b t of
            Right (term, p') -> pt3 (Negative term : terms) ops True t
            Left e           -> Left e
        TNumber n -> pt3 (Negative (Const n) : terms) ops b t
        TLower s -> pt3 (Negative (TermLower s) : terms) ops b t
        _ -> raise h
    pt3 terms ops b p@(h:t) =
      case token h of
        TPlus -> pt1 terms (TPlus : ops) b t
        TMinus -> pt1 terms (TMinus : ops) b t
        TStar -> pt1 terms (TStar : ops) b t
        TSlash -> pt1 terms (TSlash : ops) b t
        TBracketR ->
          if b
            then Right (mergeTerm (reverse terms) (reverse ops), t)
            else raise h
        tok ->
          if not b &&
             tok `elem`
             [ TCurlyR
             , TSemic
             , TEq
             , TNEq
             , TLess
             , TLEq
             , TGreater
             , TGEq
             , TAnd
             , TOr
             , TImplies
             , TEquiv
             ]
            then Right (mergeTerm (reverse terms) (reverse ops), p)
            else raise h

mkTransient :: String -> PG
mkTransient s =
  PG
    { name = s
    , variables = []
    , states = ["No", "Yes"]
    , transitions =
        [ Trans {preState = "No", guard = FTrue, action = [], postState = "No"}
        , Trans {preState = "No", guard = FTrue, action = [], postState = "Yes"}
        , Trans {preState = "Yes", guard = FTrue, action = [], postState = "No"}
        , Trans
            {preState = "Yes", guard = FTrue, action = [], postState = "Yes"}
        ]
    , initialState = "Yes"
    , initialFormula = FTrue
    , isFault = True
    }

mkPersistent :: String -> PG
mkPersistent s =
  PG
    { name = s
    , variables = []
    , states = ["No", "Yes"]
    , transitions =
        [ Trans {preState = "No", guard = FTrue, action = [], postState = "No"}
        , Trans {preState = "No", guard = FTrue, action = [], postState = "Yes"}
        ]
    , initialState = "Yes"
    , initialFormula = FTrue
    , isFault = True
    }

mergeFormula :: [Formula] -> [Token] -> Formula
mergeFormula = mf1 [] []
  where
    mf1 fa oa [f] []                = mf2 [] [] (reverse $ f : fa) (reverse oa)
    mf1 fa oa (f1:f2:fs) (TAnd:ops) = mf1 fa oa (And f1 f2 : fs) ops
    mf1 fa oa (f:fs) (o:ops)        = mf1 (f : fa) (o : oa) fs ops
    mf2 fa oa [f] []               = mf3 [] [] (f : fa) oa
    mf2 fa oa (f1:f2:fs) (TOr:ops) = mf2 fa oa (Or f1 f2 : fs) ops
    mf2 fa oa (f:fs) (o:ops)       = mf2 (f : fa) (o : oa) fs ops
    mf3 fa oa [f] []                    = mf4 (f : fa) oa
    mf3 fa oa (f1:f2:fs) (TImplies:ops) = mf3 fa oa (Implies f2 f1 : fs) ops
    mf3 fa oa (f:fs) (o:ops)            = mf3 (f : fa) (o : oa) fs ops
    mf4 [f] []             = f
    mf4 (f1:f2:fs) (o:ops) = mf4 (Equiv f1 f2 : fs) ops

mergeTerm :: [Term] -> [Token] -> Term
mergeTerm = mt1 [] []
  where
    mt1 ta oacc [t] [] = mt2 (reverse $ t : ta) (reverse oacc)
    mt1 ta oacc (t1:t2:ts) (o:ops) =
      case o of
        TStar  -> mt1 ta oacc (Multiply t1 t2 : ts) ops
        TSlash -> mt1 ta oacc (Divide t1 t2 : ts) ops
        _      -> mt1 (t1 : ta) (o : ops) (t2 : ts) ops
    mt2 [t] [] = t
    mt2 (t1:t2:ts) (o:ops) =
      case o of
        TPlus  -> mt2 (Add t1 t2 : ts) ops
        TMinus -> mt2 (Subtract t1 t2 : ts) ops

mkTrans :: State -> Formula -> Action -> State -> Trans
mkTrans s f a s' = Trans {preState = s, guard = f, action = a, postState = s'}

ignoreToken :: DToken -> Bool
ignoreToken t =
  case token t of
    TSpace     -> True
    TComment _ -> True
    TNewline   -> True
    _          -> False

raise :: DToken -> Either PError a
raise dt =
  Left
    PError
      { pMsg = "Unexpected token: " ++ show (token dt)
      , pLine = line dt
      , pCol = column dt
      }
