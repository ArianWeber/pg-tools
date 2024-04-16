module RangeCheck
  ( checkRanges
  ) where

import           AST
import qualified Data.Map as Map

checkRanges :: Env -> Model -> Maybe String
checkRanges env m = checkGraphs env $ graphs m

checkGraphs :: Env -> [PG] -> Maybe String
checkGraphs _ [] = Nothing
checkGraphs env (h:t) =
  case checkTransitions env (transitions h) of
    Nothing  -> checkGraphs env t
    Just err -> Just $ name h ++ err

checkTransitions :: Env -> [Trans] -> Maybe String
checkTransitions _ [] = Nothing
checkTransitions env (h:t) =
  case checkAction env (action h) of
    Nothing -> checkTransitions env t
    err     -> err

checkAction :: Env -> Action -> Maybe String
checkAction _ (Action []) = Nothing
checkAction env (Action (h:t)) =
  case checkAssign env h of
    Nothing -> checkAction env $ Action t
    err     -> err

checkAssign :: Env -> Assign -> Maybe String
checkAssign env (v, expr)
  | v `Map.member` eInt env =
    case expr of
      Arithmetic t -> checkTerm env v t False
      Boolean b    -> Nothing
      Single s     -> checkRange env v s False
  | otherwise = Nothing

checkTerm :: Env -> String -> Term -> Bool -> Maybe String
checkTerm env v (TermLower s) b = checkRange env v s b
checkTerm env v (Const c) False = checkValue env v c
checkTerm env v (Const c) True  = checkValue env v (-c)
checkTerm env v (Negative t) b  = checkTerm env v t (not b)
checkTerm _ _ _ _               = Nothing

checkRange :: Env -> String -> String -> Bool -> Maybe String
checkRange env x y b =
  let (r1, (l2, h2)) = (eInt env Map.! x, eInt env Map.! y)
      r2 =
        if b
          then (-h2, -l2)
          else (l2, h2)
   in if intersects r1 r2
        then Nothing
        else Just $
             ": expected assignment to " ++
             x ++
             " to be in range " ++
             show r1 ++
             ", but assigned value " ++
             y ++
             " with range " ++ show r2 ++ " does not overlap with this range"

intersects :: (Int, Int) -> (Int, Int) -> Bool
intersects (l1, h1) (l2, h2) = l1 <= h2 && l2 <= h1

checkValue :: Env -> String -> Int -> Maybe String
checkValue env s i =
  let r@(l, h) = eInt env Map.! s
   in if l <= i && i <= h
        then Nothing
        else Just $
             ": value " ++
             show i ++
             " out of range for assignment to " ++
             s ++ " which has range " ++ show r
