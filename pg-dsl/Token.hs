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
  | TBRacketL
  | TBracketR
  | TCurlyL
  | TCurlyR
  | TSquareL
  | TSquareR
  | TComma
  | TSemicolon
  | TDots
  | TArrow
  | TWalrus
  | TColon
  | TPlus
  | TMinus
  | TStar
  | TSlash
  | TEqual
  | TNotEqual
  | TLowerEq
  | TLower
  | TGreaterEq
  | TGreater
  | TNot
  | TOr
  | TAnd
  | TImplies
  | TEquiv
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
  | TState
  | TTrue
  | TFalse
  | TNumber Int
  | TNameLower String
  | TNameUpper String

instance Show Token where
  show :: Token -> String
  show TEoF           = "end of file"
  show TSpace         = "' '"
  show (TComment c)   = "'" ++ c ++ "'"
  show TNewline       = "'end of line'"
  show TBRacketL      = "'('"
  show TBracketR      = "')'"
  show TCurlyL        = "'{'"
  show TCurlyR        = "'}'"
  show TSquareL       = "'['"
  show TSquareR       = "']'"
  show TComma         = "','"
  show TSemicolon     = "';'"
  show TDots          = "'..'"
  show TArrow         = "'->'"
  show TWalrus        = "':='"
  show TColon         = "':'"
  show TPlus          = "'+'"
  show TMinus         = "'-'"
  show TStar          = "'*'"
  show TSlash         = "'/'"
  show TEqual         = "'='"
  show TNotEqual      = "'!='"
  show TLowerEq       = "'<='"
  show TLower         = "'<'"
  show TGreaterEq     = "'>='"
  show TGreater       = "'>'"
  show TNot           = "'!'"
  show TOr            = "'|'"
  show TAnd           = "'&'"
  show TImplies       = "'=>'"
  show TEquiv         = "'<=>'"
  show TGraph         = "'graph'"
  show TVars          = "'variables'"
  show TStates        = "'states'"
  show TInit          = "'init'"
  show TTransitions   = "'transitions'"
  show TGuard         = "'guard'"
  show TAction        = "'action'"
  show TBool          = "'bool'"
  show TInt           = "'int'"
  show TEnum          = "'enum'"
  show TState         = "'state'"
  show TTrue          = "'true'"
  show TFalse         = "'false'"
  show (TNumber n)    = show n
  show (TNameLower s) = "'" ++ s ++ "'"
  show (TNameUpper s) = "'" ++ s ++ "'"

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
