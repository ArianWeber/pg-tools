module AST
  ( AST
  , Model(..)
  , ProgramGraph
  , PG(..)
  , Hazard(..)
  , Spec(..)
  , State
  , VarDef(..)
  , Var
  , Range(..)
  , Value(..)
  , Trans(..)
  , Action(..)
  , Assign
  , Expression(..)
  , Term(..)
  , Formula(..)
  , LTL(..)
  , CTL(..)
  , RelOp(..)
  , ParserError(..)
  , FParserError(..)
  ) where

type AST = Either FParserError Model

data Model = Model
  { modelName :: String
  , graphs    :: [PG]
  , hazards   :: [Hazard]
  , specs     :: [Spec]
  } deriving (Show, Eq)

type ProgramGraph = Either ParserError PG

data PG = PG
  { name           :: String
  , variables      :: [VarDef]
  , states         :: [State]
  , transitions    :: [Trans]
  , initialState   :: State
  , initialFormula :: Formula
  , isFault        :: Bool
  } deriving (Show, Eq)

data Hazard =
  Hazard String (Either LTL CTL)
  deriving (Eq)

instance Show Hazard where
  show :: Hazard -> String
  show (Hazard s (Left ltl))  = s ++ ": " ++ show ltl
  show (Hazard s (Right ctl)) = s ++ ": " ++ show ctl

data Spec =
  Spec String (Either LTL CTL)
  deriving (Eq)

instance Show Spec where
  show :: Spec -> String
  show (Spec s (Left ltl))  = s ++ ": " ++ show ltl
  show (Spec s (Right ctl)) = s ++ ": " ++ show ctl

type State = String

data VarDef =
  VarDef String Range
  deriving (Show, Eq)

type Var = String

data Range
  = RBool
  | RInt Int Int -- lower and upper bound
  | REnum [String]
  deriving (Show, Eq)

data Value
  = VBool Bool
  | VInt Int
  | VEnum String
  | VState State
  deriving (Show, Eq)

data Trans = Trans
  { preState  :: State
  , guard     :: Formula
  , action    :: Action
  , postState :: State
  } deriving (Show, Eq)

newtype Action =
  Action [Assign]
  deriving (Eq)

instance Show Action where
  show :: Action -> String
  show (Action a) = sa1 a
    where
      sa1 []     = ""
      sa1 [x]    = sa2 x
      sa1 (x:xs) = sa2 x ++ "; " ++ sa1 xs
      sa2 (v, e) = v ++ " := " ++ show e

type Assign = (Var, Expression)

data Expression
  = Arithmetic Term
  | Boolean Formula
  | Single String -- single variables cannot be categorized w/o context
  deriving (Eq)

instance Show Expression where
  show :: Expression -> String
  show (Arithmetic t) = show t
  show (Boolean f)    = show f
  show (Single s)     = s

data Formula
  = FVar String
  | FTrue
  | FFalse
  | Proposition RelOp Term Term
  | Not Formula
  | And Formula Formula
  | Or Formula Formula
  | Implies Formula Formula
  | Equiv Formula Formula
  deriving (Eq)

instance Show Formula where
  show :: Formula -> String
  show (FVar s)            = s
  show FTrue               = "true"
  show FFalse              = "false"
  show (Proposition r a b) = show a ++ " " ++ show r ++ " " ++ show b
  show (Not a)             = "! " ++ show a
  show (And a b)           = "(" ++ show a ++ " && " ++ show b ++ ")"
  show (Or a b)            = "(" ++ show a ++ " || " ++ show b ++ ")"
  show (Implies a b)       = "(" ++ show a ++ " => " ++ show b ++ ")"
  show (Equiv a b)         = "(" ++ show a ++ " <=> " ++ show b ++ ")"

data LTL
  = LTLVar String
  | LTLTrue
  | LTLFalse
  | LTLProp RelOp Term Term
  | LTLNot LTL
  | LTLAnd LTL LTL
  | LTLOr LTL LTL
  | LTLImplies LTL LTL
  | LTLEquiv LTL LTL
  | LTLX LTL
  | LTLF LTL
  | LTLG LTL
  | LTLU LTL LTL
  deriving (Eq)

instance Show LTL where
  show :: LTL -> String
  show (LTLVar s)       = s
  show LTLTrue          = "true"
  show LTLFalse         = "false"
  show (LTLProp r a b)  = show a ++ " " ++ show r ++ " " ++ show b
  show (LTLNot a)       = "! " ++ show a
  show (LTLAnd a b)     = "(" ++ show a ++ " && " ++ show b ++ ")"
  show (LTLOr a b)      = "(" ++ show a ++ " || " ++ show b ++ ")"
  show (LTLImplies a b) = "(" ++ show a ++ " => " ++ show b ++ ")"
  show (LTLEquiv a b)   = "(" ++ show a ++ " <=> " ++ show b ++ ")"
  show (LTLX a)         = "X " ++ show a
  show (LTLF a)         = "F " ++ show a
  show (LTLG a)         = "G " ++ show a
  show (LTLU a b)       = "(" ++ show a ++ " U " ++ show b ++ ")"

data CTL
  = CTLVar String
  | CTLTrue
  | CTLFalse
  | CTLProp RelOp Term Term
  | CTLNot CTL
  | CTLAnd CTL CTL
  | CTLOr CTL CTL
  | CTLImplies CTL CTL
  | CTLEquiv CTL CTL
  | CTLEX CTL
  | CTLEF CTL
  | CTLEG CTL
  | CTLEU CTL CTL
  | CTLAX CTL
  | CTLAF CTL
  | CTLAG CTL
  | CTLAU CTL CTL
  deriving (Eq)

instance Show CTL where
  show :: CTL -> String
  show (CTLVar s)       = s
  show CTLTrue          = "true"
  show CTLFalse         = "false"
  show (CTLProp r a b)  = show a ++ " " ++ show r ++ " " ++ show b
  show (CTLNot a)       = "! " ++ show a
  show (CTLAnd a b)     = "(" ++ show a ++ " && " ++ show b ++ ")"
  show (CTLOr a b)      = "(" ++ show a ++ " || " ++ show b ++ ")"
  show (CTLImplies a b) = "(" ++ show a ++ " => " ++ show b ++ ")"
  show (CTLEquiv a b)   = "(" ++ show a ++ " <=> " ++ show b ++ ")"
  show (CTLEX a)        = "EX " ++ show a
  show (CTLEF a)        = "EF " ++ show a
  show (CTLEG a)        = "EG " ++ show a
  show (CTLEU a b)      = "(" ++ show a ++ " EU " ++ show b ++ ")"
  show (CTLAX a)        = "AX " ++ show a
  show (CTLAF a)        = "AF " ++ show a
  show (CTLAG a)        = "AG " ++ show a
  show (CTLAU a b)      = "(" ++ show a ++ " AU " ++ show b ++ ")"

data Term
  = TermLower String
  | TermUpper String
  | Const Int
  | Negative Term
  | Add Term Term
  | Subtract Term Term
  | Multiply Term Term
  | Divide Term Term
  deriving (Eq)

instance Show Term where
  show :: Term -> String
  show (TermLower s) = s
  show (TermUpper s) = s
  show (Const i) = show i
  show (Negative a) =
    case a of
      (Negative _) -> "- (" ++ show a ++ ")"
      (Const i) ->
        if i < 0
          then "-(" ++ show a ++ ")"
          else "-" ++ show a
      _ -> "-" ++ show a
  show (Add a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (Subtract a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
  show (Multiply a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
  show (Divide a b) = "(" ++ show a ++ " / " ++ show b ++ ")"

data RelOp
  = Equal
  | NotEq
  | Less
  | LessEq
  | Greater
  | GreaterEq
  deriving (Eq)

instance Show RelOp where
  show :: RelOp -> String
  show Equal     = "=="
  show NotEq     = "!="
  show Less      = "<"
  show LessEq    = "<="
  show Greater   = ">"
  show GreaterEq = ">="

data ParserError = ParserError
  { pMsg  :: String
  , pLine :: Int
  , pCol  :: Int
  }

instance Show ParserError where
  show :: ParserError -> String
  show e =
    pMsg e ++ " at line " ++ show (pLine e) ++ ", column " ++ show (pCol e)

data FParserError =
  FPError FilePath ParserError

instance Show FParserError where
  show :: FParserError -> String
  show (FPError fp err) = show err ++ " in file " ++ fp
