module AST
  ( AST
  , ProgramGraph
  , PG(..)
  , State
  , VarDef
  , Var
  , Range(..)
  , Value(..)
  , Trans(..)
  , Action
  , Assign
  , Expression(..)
  , Term(..)
  , Formula(..)
  , RelOp(..)
  , PError(..)
  ) where

type AST = [ProgramGraph]

type ProgramGraph = Either PError PG

data PG = PG
  { name           :: String
  , variables      :: [VarDef]
  , states         :: [State]
  , transitions    :: [Trans]
  , initialState   :: State
  , initialFormula :: Formula
  } deriving (Show, Eq)

type State = String

type VarDef = (String, Range)

type Var = String

data Range
  = RBool
  | RInt Int Int -- lower and upper bound
  | REnum [String]
  | RState
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

type Action = [Assign]

type Assign = (Var, Expression)

data Expression
  = Arithmetic Term
  | Boolean Formula
  | Single String -- single variables cannot be categorized w/o context
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data Term
  = TermLower String
  | TermUpper String
  | Const Int
  | Negative Term
  | Add Term Term
  | Subtract Term Term
  | Multiply Term Term
  | Divide Term Term
  deriving (Show, Eq)

data RelOp
  = Equal
  | NotEq
  | Less
  | LessEq
  | Greater
  | GreaterEq
  deriving (Show, Eq)

data PError = PError
  { pMsg  :: String
  , pLine :: Int
  , pCol  :: Int
  }

instance Show PError where
  show :: PError -> String
  show e =
    pMsg e ++ " at line " ++ show (pLine e) ++ ", column " ++ show (pCol e)
