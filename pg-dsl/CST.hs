module CST
  ( Env(..)
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

data Env = Env
  { eBool  :: Set.Set String
  , eInt   :: Map.Map String (Int, Int)
  , eEnum  :: Map.Map String [String]
  , eGraph :: Map.Map String [String]
  } deriving (Show, Eq)
