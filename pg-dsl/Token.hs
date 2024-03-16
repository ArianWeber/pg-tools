module Token
  ( TokenList
  , Token(..)
  , DToken(..)
  , TError(..)
  ) where

type TokenList = Either TError [DToken]

-- TODO: show original chars
data Token
  = TEoF
  | TSpace
  | TComment String
  | TNewline
  | TBracketL
  | TBracketR
  | TCurlyL
  | TCurlyR
  | TSquareL
  | TSquareR
  | TComma
  | TSemic
  | TDots
  | TArrow
  | TWalrus
  | TColon
  | TPlus
  | TMinus
  | TStar
  | TSlash
  | TEq
  | TNEq
  | TLEq
  | TLess
  | TGEq
  | TGreater
  | TNot
  | TOr
  | TAnd
  | TImplies
  | TEquiv
  | TX
  | TF
  | TG
  | TU
  | TEX
  | TEF
  | TEG
  | TEU
  | TAX
  | TAF
  | TAG
  | TAU
  | TGraph
  | TVars
  | TStates
  | TInit
  | TTransitions
  | TGuard
  | TAction
  | TBool
  | TInt
  | TEnum
  | TTrue
  | TFalse
  | TModel
  | TErrors
  | TTransient
  | TPersistent
  | THazards
  | TSpecify
  | TString String
  | TNumber Int
  | TLower String
  | TUpper String
  deriving (Eq)

instance Show Token where
  show :: Token -> String
  show TEoF         = "end of file"
  show TSpace       = "' '"
  show (TComment c) = "'" ++ c ++ "'"
  show TNewline     = "newline"
  show TBracketL    = "'('"
  show TBracketR    = "')'"
  show TCurlyL      = "'{'"
  show TCurlyR      = "'}'"
  show TSquareL     = "'['"
  show TSquareR     = "']'"
  show TComma       = "','"
  show TSemic       = "';'"
  show TDots        = "'..'"
  show TArrow       = "'->'"
  show TWalrus      = "':='"
  show TColon       = "':'"
  show TPlus        = "'+'"
  show TMinus       = "'-'"
  show TStar        = "'*'"
  show TSlash       = "'/'"
  show TEq          = "'='"
  show TNEq         = "'!='"
  show TLEq         = "'<='"
  show TLess        = "'<'"
  show TGEq         = "'>='"
  show TGreater     = "'>'"
  show TNot         = "'!'"
  show TOr          = "'|'"
  show TAnd         = "'&'"
  show TImplies     = "'=>'"
  show TEquiv       = "'<=>'"
  show TX           = "'N'"
  show TF           = "'F'"
  show TG           = "'G'"
  show TU           = "'U'"
  show TEX          = "'EN'"
  show TEF          = "'EF'"
  show TEG          = "'EG'"
  show TEU          = "'EU'"
  show TAX          = "'AX'"
  show TAF          = "'AF'"
  show TAG          = "'AG'"
  show TAU          = "'AU'"
  show TGraph       = "'graph'"
  show TVars        = "'variables'"
  show TStates      = "'states'"
  show TInit        = "'init'"
  show TTransitions = "'transitions'"
  show TGuard       = "'guard'"
  show TAction      = "'action'"
  show TBool        = "'bool'"
  show TInt         = "'int'"
  show TEnum        = "'enum'"
  show TTrue        = "'true'"
  show TFalse       = "'false'"
  show TModel       = "'model'"
  show TErrors      = "'errors'"
  show TTransient   = "'transient'"
  show TPersistent  = "'persistent'"
  show THazards     = "'hazards'"
  show TSpecify     = "'specify'"
  show (TString s)  = show s
  show (TNumber n)  = "'" ++ show n ++ "'"
  show (TLower s)   = "'" ++ s ++ "'"
  show (TUpper s)   = "'" ++ s ++ "'"

data DToken = DToken
  { token  :: Token
  , line   :: Int
  , column :: Int
  }

instance Show DToken where
  show :: DToken -> String
  show t =
    "(" ++
    show (token t) ++ ", " ++ show (line t) ++ ", " ++ show (column t) ++ ")"

data TError = TError
  { tMsg  :: String
  , tLine :: Int
  , tCol  :: Int
  }

instance Show TError where
  show :: TError -> String
  show e =
    tMsg e ++ " at line " ++ show (tLine e) ++ ", column " ++ show (tCol e)
