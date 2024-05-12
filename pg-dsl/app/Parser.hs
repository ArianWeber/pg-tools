module Parser
  ( parse
  , parseMain
  ) where

import           AST
import           Token

parseMain :: TokenList -> Either ParserError Model
parseMain (Left e) =
  Left $ ParserError {pMsg = tMsg e, pLine = tLine e, pCol = tCol e}
parseMain (Right t) =
  case pMain (filter (not . ignoreToken) t) of
    Left e -> Left e
    Right (pg, h:_) ->
      case token h of
        TEoF -> Right pg
        _    -> raise h

parse :: TokenList -> ProgramGraph
parse (Left e) =
  Left $ ParserError {pMsg = tMsg e, pLine = tLine e, pCol = tCol e}
parse (Right t) =
  case pGraph (filter (not . ignoreToken) t) of
    Left e -> Left e
    Right (pg, h:_) ->
      case token h of
        TEoF -> Right pg
        _    -> raise h

pMain :: [DToken] -> Either ParserError (Model, [DToken])
pMain (h:t) =
  case token h of
    TModel -> pm1 t
    _      -> raise h
  where
    pm1 (h:t) =
      case token h of
        TUpper s -> pm2 s t
        _        -> raise h
    pm2 s (h:t) =
      case token h of
        TCurlyL -> pm3 s t
        _       -> raise h
    pm3 s p =
      case pErrors p of
        Right (g, p') -> pm4 g s p'
        Left e        -> Left e
    pm4 g s p =
      case pHazards p of
        Right (haz, p') -> pm5 haz g s p'
        Left e          -> Left e
    pm5 haz g s p =
      case pSpecs p of
        Right (sp, p') -> pm6 sp haz g s p'
        Left e         -> Left e
    pm6 sp haz g s (h:t) =
      case token h of
        TCurlyR ->
          Right
            ( Model
                { modelName = s
                , graphs = g
                , hazards = haz
                , specs = sp
                , environ = emptyEnv
                }
            , t)
        _ -> raise h

pErrors :: [DToken] -> Either ParserError ([PG], [DToken])
pErrors p@(h:t) =
  case token h of
    TErrors -> pe1 t
    _       -> Right ([], p)
  where
    pe1 (h:t) =
      case token h of
        TCurlyL -> pe2 [] t
        _       -> raise h
    pe2 acc (h:t) =
      case token h of
        TCurlyR     -> Right (acc, t)
        TTransient  -> pe3 False acc t
        TPersistent -> pe3 True acc t
        _           -> raise h
    pe3 persistent acc (h:t) =
      case token h of
        TUpper s ->
          if persistent
            then pe2 (mkPersistent s : acc) t
            else pe2 (mkTransient s : acc) t
        _ -> raise h

pHazards :: [DToken] -> Either ParserError ([Hazard], [DToken])
pHazards p@(h:t) =
  case token h of
    THazards -> ph1 t
    _        -> Right ([], p)
  where
    ph1 (h:t) =
      case token h of
        TCurlyL -> ph2 [] t
        _       -> raise h
    ph2 acc (h:t) =
      case token h of
        TCurlyR   -> Right (acc, t)
        TString s -> ph3 s acc t
        _         -> raise h
    ph3 s acc (h:t) =
      case token h of
        TCurlyL -> ph4 s acc t
        _       -> raise h
    ph4 s acc p =
      case pLTL p of
        Right (ltl, p') -> ph5 (Hazard s (Left ltl) : acc) p'
        Left _ ->
          case pCTL p of
            Right (ctl, p') -> ph5 (Hazard s (Right ctl) : acc) p'
            Left e          -> Left e
    ph5 acc (h:t) =
      case token h of
        TCurlyR -> ph2 acc t
        _       -> raise h

pSpecs :: [DToken] -> Either ParserError ([Spec], [DToken])
pSpecs p@(h:t) =
  case token h of
    TSpecify -> ph1 t
    _        -> Right ([], p)
  where
    ph1 (h:t) =
      case token h of
        TCurlyL -> ph2 [] t
        _       -> raise h
    ph2 acc (h:t) =
      case token h of
        TCurlyR   -> Right (acc, t)
        TString s -> ph3 s acc t
        _         -> raise h
    ph3 s acc (h:t) =
      case token h of
        TCurlyL -> ph4 s acc t
        _       -> raise h
    ph4 s acc p =
      case pLTL p of
        Right (ltl, p') -> ph5 (Spec s (Left ltl) : acc) p'
        Left _ ->
          case pCTL p of
            Right (ctl, p') -> ph5 (Spec s (Right ctl) : acc) p'
            Left e          -> Left e
    ph5 acc (h:t) =
      case token h of
        TCurlyR -> ph2 acc t
        _       -> raise h

pGraph :: [DToken] -> Either ParserError (PG, [DToken])
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

pVars :: [DToken] -> Either ParserError ([VarDef], [DToken])
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
        TCurlyR -> Right (vs, t)
        _ -> raise h

pBool :: [DToken] -> Either ParserError (VarDef, [DToken])
pBool (h:t) =
  case token h of
    TLower "nofaults" -> raise h
    TLower s          -> Right (VarDef s RBool, t)
    _                 -> raise h

pInt :: [DToken] -> Either ParserError (VarDef, [DToken])
pInt (h:t) =
  case token h of
    TLower "nofaults" -> raise h
    TLower s          -> pi1 s t
    _                 -> raise h
  where
    pi1 s (h:t) =
      case token h of
        TSquareL -> pi2 s t
        _        -> raise h
    pi2 s (h:t) =
      case token h of
        TMinus    -> pi2' s t
        TNumber i -> pi3 i s t
        _         -> raise h
    pi2' s (h:t) =
      case token h of
        TNumber i -> pi3 (-i) s t
        _         -> raise h
    pi3 i s (h:t) =
      case token h of
        TDots -> pi4 i s t
        _     -> raise h
    pi4 i s (h:t) =
      case token h of
        TMinus    -> pi4' i s t
        TNumber j -> pi5 i j s t
        _         -> raise h
    pi4' i s (h:t) =
      case token h of
        TNumber j -> pi5 i (-j) s t
        _         -> raise h
    pi5 i j s (h:t) =
      case token h of
        TSquareR -> Right (VarDef s (RInt (min i j) (max i j)), t)
        _        -> raise h

pEnum :: [DToken] -> Either ParserError (VarDef, [DToken])
pEnum (h:t) =
  case token h of
    TLower "nofaults" -> raise h
    TLower s          -> pe1 s t
    _                 -> raise h
  where
    pe1 s (h:t) =
      case token h of
        TCurlyL -> pe2 [] s t
        _       -> raise h
    pe2 vs s (h:t) =
      case token h of
        TLower "nofaults" -> raise h
        TLower v          -> pe3 (v : vs) s t
        _                 -> raise h
    pe3 vs s (h:t) =
      case token h of
        TComma  -> pe2 vs s t
        TCurlyR -> Right (VarDef s (REnum vs), t)
        _       -> raise h

pStates :: [DToken] -> Either ParserError ([State], [DToken])
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

pInit :: [DToken] -> Either ParserError ((State, Formula), [DToken])
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

pTransitions :: [DToken] -> Either ParserError ([Trans], [DToken])
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

pTrans :: [DToken] -> Either ParserError (Trans, [DToken])
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
        TCurlyR -> Right (mkTrans s FTrue (Action []) s', t)
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

pAction :: [DToken] -> Either ParserError (Action, [DToken])
pAction p@(h:t) =
  case token h of
    TAction -> pa1 t
    TCurlyR -> Right (Action [], p)
    _       -> raise h
  where
    pa1 (h:t) =
      case token h of
        TCurlyL -> pa2 [] t
        _       -> raise h
    pa2 acc (h:t) =
      case token h of
        TLower "nofaults" -> raise h
        TLower s          -> pa3 s acc t
        TCurlyR           -> Right (Action acc, t)
        _                 -> raise h
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
        TCurlyR -> Right (Action acc, t)
        TSemic  -> pa2 acc t
        _       -> raise h

pExpression :: [DToken] -> Either ParserError (Expression, [DToken])
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

pSingle :: [DToken] -> Either ParserError (String, [DToken])
pSingle (h:t) =
  case token h of
    TLower "nofaults" -> raise h
    TLower s          -> ps1 s t
    _                 -> raise h
  where
    ps1 s p@(h:_) =
      case token h of
        TCurlyR -> Right (s, p)
        TSemic  -> Right (s, p)
        _       -> raise h

pFormula :: [DToken] -> Either ParserError (Formula, [DToken])
pFormula = pf1 [] []
  where
    pf1 fs ops p@(h:t) =
      case pProposition p of
        Right (f, p') -> pf2 (uncurry3 Proposition f : fs) ops p'
        Left _ ->
          case token h of
            TBracketL ->
              case pFormula t of
                Right (f, h':t') ->
                  case token h' of
                    TBracketR -> pf2 (f : fs) ops t'
                    _         -> raise h'
                Left e -> Left e
            _ ->
              case pfNext p of
                Right (f, p') -> pf2 (f : fs) ops p'
                Left e        -> Left e
    pf2 fs ops p@(h:t) =
      case token h of
        TAnd     -> pf1 fs (TAnd : ops) t
        TOr      -> pf1 fs (TOr : ops) t
        TImplies -> pf1 fs (TImplies : ops) t
        TEquiv   -> pf1 fs (TEquiv : ops) t
        _        -> Right (mergeFormula fs ops, p)
    pfNext (h:t) =
      case token h of
        TTrue -> Right (FTrue, t)
        TFalse -> Right (FFalse, t)
        TLower "nofaults" -> raise h
        TLower s -> Right (FVar s, t)
        TNot ->
          case pf3 t of
            Right (f, p') -> Right (Not f, p')
            Left e        -> Left e
        _ -> raise h
    pf3 p@(h:t) =
      case pProposition p of
        Right (f, p') -> Right (uncurry3 Proposition f, p')
        Left _ ->
          case token h of
            TBracketL ->
              case pFormula p of
                Right (f, p') -> Right (f, p')
                Left e        -> Left e
            _ ->
              case pfNext p of
                Right (f, p') -> Right (f, p')
                Left e        -> Left e

pLTL :: [DToken] -> Either ParserError (LTL, [DToken])
pLTL = pl1 [] []
  where
    pl1 fs ops p@(h:t) =
      case pProposition p of
        Right (f, p') -> pl2 (uncurry3 LTLProp f : fs) ops p'
        Left _ ->
          case token h of
            TBracketL ->
              case pLTL t of
                Right (f, h':t') ->
                  case token h' of
                    TBracketR -> pl2 (f : fs) ops t'
                    _         -> raise h'
                Left e -> Left e
            _ ->
              case plNext p of
                Right (f, p') -> pl2 (f : fs) ops p'
                Left e        -> Left e
    pl2 fs ops p@(h:t) =
      case token h of
        TAnd     -> pl1 fs (TAnd : ops) t
        TOr      -> pl1 fs (TOr : ops) t
        TImplies -> pl1 fs (TImplies : ops) t
        TEquiv   -> pl1 fs (TEquiv : ops) t
        TU       -> pl1 fs (TU : ops) t
        _        -> Right (mergeLTL fs ops, p)
    plNext (h:t) =
      case token h of
        TTrue    -> Right (LTLTrue, t)
        TFalse   -> Right (LTLFalse, t)
        TLower s -> Right (LTLVar s, t)
        TNot     -> pl3 LTLNot t
        TX       -> pl3 LTLX t
        TF       -> pl3 LTLF t
        TG       -> pl3 LTLG t
        _        -> raise h
    pl3 c p =
      case pl4 p of
        Right (f, p') -> Right (c f, p')
        Left e        -> Left e
    pl4 p@(h:t) =
      case pProposition p of
        Right (f, p') -> Right (uncurry3 LTLProp f, p')
        Left _ ->
          case token h of
            TBracketL ->
              case pLTL p of
                Right (f, p') -> Right (f, p')
                Left e        -> Left e
            _ ->
              case plNext p of
                Right (f, p') -> Right (f, p')
                Left e        -> Left e

pCTL :: [DToken] -> Either ParserError (CTL, [DToken])
pCTL = pc1 [] []
  where
    pc1 fs ops p@(h:t) =
      case pProposition p of
        Right (f, p') -> pc2 (uncurry3 CTLProp f : fs) ops p'
        Left _ ->
          case token h of
            TBracketL ->
              case pCTL t of
                Right (f, h':t') ->
                  case token h' of
                    TBracketR -> pc2 (f : fs) ops t'
                    _         -> raise h'
                Left e -> Left e
            _ ->
              case pcNext p of
                Right (f, p') -> pc2 (f : fs) ops p'
                Left e        -> Left e
    pc2 fs ops p@(h:t) =
      case token h of
        TAnd     -> pc1 fs (TAnd : ops) t
        TOr      -> pc1 fs (TOr : ops) t
        TImplies -> pc1 fs (TImplies : ops) t
        TEquiv   -> pc1 fs (TEquiv : ops) t
        TEU      -> pc1 fs (TEU : ops) t
        TAU      -> pc1 fs (TAU : ops) t
        _        -> Right (mergeCTL fs ops, p)
    pcNext (h:t) =
      case token h of
        TTrue    -> Right (CTLTrue, t)
        TFalse   -> Right (CTLFalse, t)
        TLower s -> Right (CTLVar s, t)
        TNot     -> pc3 CTLNot t
        TEX      -> pc3 CTLEX t
        TEF      -> pc3 CTLEF t
        TEG      -> pc3 CTLEG t
        TAX      -> pc3 CTLAX t
        TAF      -> pc3 CTLAF t
        TAG      -> pc3 CTLAG t
        _        -> raise h
    pc3 c p =
      case pc4 p of
        Right (f, p') -> Right (c f, p')
        Left e        -> Left e
    pc4 p@(h:t) =
      case pProposition p of
        Right (f, p') -> Right (uncurry3 CTLProp f, p')
        Left _ ->
          case token h of
            TBracketL ->
              case pCTL p of
                Right (f, p') -> Right (f, p')
                Left e        -> Left e
            _ ->
              case pcNext p of
                Right (f, p') -> Right (f, p')
                Left e        -> Left e

pProposition :: [DToken] -> Either ParserError ((RelOp, Term, Term), [DToken])
pProposition p@(h:t) =
  case token h of
    TBracketL ->
      case pProposition t of
        Right (pr, h':t') ->
          case token h' of
            TBracketR -> Right (pr, t')
            _         -> raise h'
        Left e -> Left e
    _ ->
      case pTerm p of
        Right (t1, p') -> pp1 t1 p'
        Left e         -> Left e
  where
    pp1 t1 (h:t) =
      case token h of
        TEq      -> pp2 Equal t1 t
        TNEq     -> pp2 NotEq t1 t
        TLess    -> pp2 Less t1 t
        TLEq     -> pp2 LessEq t1 t
        TGreater -> pp2 Greater t1 t
        TGEq     -> pp2 GreaterEq t1 t
        _        -> raise h
    pp2 op t1 p =
      case pTerm p of
        Right (t2, p') -> Right ((op, t1, t2), p')
        Left e         -> Left e

pTerm :: [DToken] -> Either ParserError (Term, [DToken])
pTerm = pt1 [] []
  where
    pt1 terms ops p@(h:t) =
      case token h of
        TBracketL ->
          case pTerm p of
            Right (te, h':t') ->
              case token h' of
                TBracketR -> pt2 (te : terms) ops t'
                _         -> raise h'
            Left e -> Left e
        _ ->
          case ptNext p of
            Right (te, p') -> pt2 (te : terms) ops p'
            Left e         -> Left e
    pt2 terms ops p@(h:t) =
      case token h of
        TPlus  -> pt1 terms (TPlus : ops) t
        TMinus -> pt1 terms (TMinus : ops) t
        TStar  -> pt1 terms (TStar : ops) t
        TSlash -> pt1 terms (TSlash : ops) t
        _      -> Right (mergeTerm terms ops, p)
    ptNext (h:t) =
      case token h of
        TNumber n -> Right (Const n, t)
        TLower "nofaults" -> raise h
        TLower s -> Right (TermLower s, t)
        TUpper s -> Right (TermUpper s, t)
        TMinus ->
          case pTerm t of
            Right (Const c, p') -> Right (Const (-c), p')
            Right (term, p')    -> Right (Negative term, p')
            Left e              -> Left e
        _ -> raise h

mkTransient :: String -> PG
mkTransient s =
  PG
    { name = s
    , variables = []
    , states = ["No", "Yes"]
    , transitions =
        [ mkTrans "No" FTrue (Action []) "No"
        , mkTrans "No" FTrue (Action []) "Yes"
        , mkTrans "Yes" FTrue (Action []) "No"
        , mkTrans "Yes" FTrue (Action []) "Yes"
        ]
    , initialState = "No"
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
        [ mkTrans "No" FTrue (Action []) "No"
        , mkTrans "No" FTrue (Action []) "Yes"
        ]
    , initialState = "No"
    , initialFormula = FTrue
    , isFault = True
    }

mergeFormula :: [Formula] -> [Token] -> Formula
mergeFormula fs ops = mf1 [] [] (reverse fs) (reverse ops)
  where
    mf1 fa oa [f] []                = mf2 [] [] (reverse $ f : fa) (reverse oa)
    mf1 fa oa (f1:f2:fs) (TAnd:ops) = mf1 fa oa (And f1 f2 : fs) ops
    mf1 fa oa (f:fs) (o:ops)        = mf1 (f : fa) (o : oa) fs ops
    mf2 fa oa [f] []               = mf3 [] [] (f : fa) oa
    mf2 fa oa (f1:f2:fs) (TOr:ops) = mf2 fa oa (Or f1 f2 : fs) ops
    mf2 fa oa (f:fs) (o:ops)       = mf2 (f : fa) (o : oa) fs ops
    mf3 fa oa [f] []                    = mf4 (f : fa)
    mf3 fa oa (f1:f2:fs) (TImplies:ops) = mf3 fa oa (Implies f2 f1 : fs) ops
    mf3 fa oa (f:fs) (o:ops)            = mf3 (f : fa) (o : oa) fs ops
    mf4 [f]        = f
    mf4 (f1:f2:fs) = mf4 (Equiv f1 f2 : fs)

mergeLTL :: [LTL] -> [Token] -> LTL
mergeLTL fs ops = ml1 [] [] (reverse fs) (reverse ops)
  where
    ml1 fa oa [f] []              = ml2 [] [] (f : fa) oa
    ml1 fa oa (f1:f2:fs) (TU:ops) = ml1 fa oa (LTLU f1 f2 : fs) ops
    ml1 fa oa (f:fs) (o:ops)      = ml1 (f : fa) (o : oa) fs ops
    ml2 fa oa [f] []                = ml3 [] [] (f : fa) oa
    ml2 fa oa (f1:f2:fs) (TAnd:ops) = ml2 fa oa (LTLAnd f1 f2 : fs) ops
    ml2 fa oa (f:fs) (o:ops)        = ml2 (f : fa) (o : oa) fs ops
    ml3 fa oa [f] []               = ml4 [] [] (f : fa) oa
    ml3 fa oa (f1:f2:fs) (TOr:ops) = ml3 fa oa (LTLOr f1 f2 : fs) ops
    ml3 fa oa (f:fs) (o:ops)       = ml3 (f : fa) (o : oa) fs ops
    ml4 fa oa [f] []                    = ml5 (f : fa)
    ml4 fa oa (f1:f2:fs) (TImplies:ops) = ml4 fa oa (LTLImplies f2 f1 : fs) ops
    ml4 fa oa (f:fs) (o:ops)            = ml4 (f : fa) (o : oa) fs ops
    ml5 [f]        = f
    ml5 (f1:f2:fs) = ml5 (LTLEquiv f1 f2 : fs)

mergeCTL :: [CTL] -> [Token] -> CTL
mergeCTL fs ops = mc1 [] [] (reverse fs) (reverse ops)
  where
    mc1 :: [CTL] -> [Token] -> [CTL] -> [Token] -> CTL
    mc1 fa oa [f] []               = mc2 [] [] (reverse $ f : fa) (reverse oa)
    mc1 fa oa (f1:f2:fs) (TEU:ops) = mc1 fa oa (CTLEU f1 f2 : fs) ops
    mc1 fa oa (f1:f2:fs) (TAU:ops) = mc1 fa oa (CTLAU f1 f2 : fs) ops
    mc1 fa oa (f:fs) (o:ops)       = mc1 (f : fa) (o : oa) fs ops
    mc2 fa oa [f] []                = mc3 [] [] (reverse $ f : fa) (reverse oa)
    mc2 fa oa (f1:f2:fs) (TAnd:ops) = mc2 fa oa (CTLAnd f1 f2 : fs) ops
    mc2 fa oa (f:fs) (o:ops)        = mc2 (f : fa) (o : oa) fs ops
    mc3 fa oa [f] []               = mc4 [] [] (f : fa) oa
    mc3 fa oa (f1:f2:fs) (TOr:ops) = mc3 fa oa (CTLOr f1 f2 : fs) ops
    mc3 fa oa (f:fs) (o:ops)       = mc3 (f : fa) (o : oa) fs ops
    mc4 fa oa [f] []                    = mc5 (f : fa)
    mc4 fa oa (f1:f2:fs) (TImplies:ops) = mc4 fa oa (CTLImplies f2 f1 : fs) ops
    mc4 fa oa (f:fs) (o:ops)            = mc4 (f : fa) (o : oa) fs ops
    mc5 [f]        = f
    mc5 (f1:f2:fs) = mc5 (CTLEquiv f1 f2 : fs)

mergeTerm :: [Term] -> [Token] -> Term
mergeTerm ts ops = mt1 [] [] (reverse ts) (reverse ops)
  where
    mt1 ta oa [t] [] = mt2 (reverse $ t : ta) (reverse oa)
    mt1 ta oa (t1:t2:ts) (o:ops) =
      case o of
        TStar  -> mt1 ta oa (Multiply t1 t2 : ts) ops
        TSlash -> mt1 ta oa (Divide t1 t2 : ts) ops
        _      -> mt1 (t1 : ta) (o : oa) (t2 : ts) ops
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

raise :: DToken -> Either ParserError a
raise dt =
  Left
    ParserError
      { pMsg = "Unexpected token: " ++ show (token dt)
      , pLine = line dt
      , pCol = column dt
      }

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
