{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore #-}

module Compiler where

import qualified Data.Map as M
import Data.List (transpose, nub)

type Ctx = M.Map String [Constr]

data Constr
  = Constr {name :: String, arity :: Int, ty :: String}
  deriving (Eq, Ord)

instance Show Constr where
  show (Constr n a t) = t ++ "." ++ n ++ " " ++ show a

exConstr1 :: Constr
exConstr1 = Constr "nil" 0 "List"

exConstr2 :: Constr
exConstr2 = Constr "cons" 2 "List"

exConstr3 :: Constr
exConstr3 = Constr "Unit" 0 "Unit"


data Val
  = ValConstr Constr [Val]
  deriving (Show, Eq, Ord)

exVal0 :: Val
exVal0 = ValConstr exConstr3 []

exVal1 :: Val
exVal1 = ValConstr exConstr1 []

exVal2 :: Val
exVal2 = ValConstr exConstr2 [exVal0, exVal1]


data Pat
  = PatWild
  | PatConstr Constr [Pat]
  | PatOr Pat Pat
  deriving (Eq)

instance Show Pat where
  show PatWild = "_"
  show (PatConstr c ps) = show c ++ " " ++ show ps
  show (PatOr p1 p2) = show p1 ++ " | " ++ show p2

exPat1 :: Pat
exPat1 = PatConstr exConstr1 []

exPat2 :: Pat
exPat2 = PatConstr exConstr2 [PatWild, PatWild]



perm :: Eq a => [a] -> [a] -> Bool
perm xs ys = all (\x -> x `elem` ys) xs && all (\y -> y `elem` xs) ys

sig :: [Constr] -> Ctx -> Bool
sig [] _ = False
sig cs ctx =
  case nub (map ty cs) of
    [c] -> 
      case M.lookup c ctx of
        Just x -> perm x cs
        Nothing -> False
    _ -> False




instOf :: Pat -> Val -> Bool
instOf PatWild _ = True
instOf (PatOr p1 p2) v = instOf p1 v || instOf p2 v
instOf (PatConstr c ps) v =
  case v of
    ValConstr c' vs ->
      if c == c' then 
        (case (ps, vs) of
          ([], []) -> True
          ([], _) -> False
          (_, []) -> False
          (p' : ps', v' : vs') ->
            instOf p' v' && instOf (PatConstr c ps') (ValConstr c' vs'))
      else False

listInstOf :: [Pat] -> [Val] -> Bool
listInstOf ps vs = and (zipWith instOf ps vs)

type Action = Int
type Matrix a = [[a]]
type ClauseMatrix = (Matrix Pat, [Action])


exMatrix1 :: Matrix Pat
exMatrix1 = [[exPat1, PatWild], [PatWild, exPat1], [exPat2, exPat2]]

exMatrix2 :: Matrix Pat
exMatrix2 = [[exPat1, PatWild], [PatWild, exPat1], [PatWild, PatWild]]

exActions :: [Action]
exActions = [1, 2, 3]

exClause1 :: ClauseMatrix
exClause1 = (exMatrix1, exActions)

exClause2 :: ClauseMatrix
exClause2 = (exMatrix2, exActions)


mlMatch :: [Val] -> ClauseMatrix -> Maybe Action
mlMatch vs m =
  case m of
    (l : ls, a : as) ->
      if listInstOf l vs then
        Just a
      else
        mlMatch vs (ls, as)
    (_, _) -> Nothing



{- 
  Matrix Decomposition
-}

lineSpec :: Constr -> [Pat] -> Action -> Maybe ClauseMatrix
lineSpec _ [] _ = Nothing
lineSpec c (p:ps) a =
  case p of
    PatWild -> Just ([replicate (arity c) PatWild ++ ps], [a])
    PatConstr c' ps' ->
      if c == c' then Just ([ps' ++ ps], [a])
      else Nothing
    PatOr p1 p2 ->
      case (m1, m2) of
        (Nothing, Nothing) -> Nothing
        (Just m1', Nothing) -> Just m1'
        (Nothing, Just m2') -> Just m2'
        (Just m1', Just m2') -> Just (m1' <> m2')
      where
        m1 = lineSpec c (p1:ps) a
        m2 = lineSpec c (p2:ps) a

spec :: Constr -> ClauseMatrix -> ClauseMatrix
spec c m =
  case m of
    (l : ls, a : as) ->
      case newLine of
        Nothing -> spec c (ls, as)
        Just x -> x <> spec c (ls, as)
      where
        newLine = lineSpec c l a
    (_, _) -> ([], [])


lineDefault :: [Pat] -> Action -> Maybe ClauseMatrix
lineDefault [] _ = Nothing
lineDefault (p:ps) a =
  case p of
    PatWild -> Just ([ps], [a])
    PatConstr _ _ -> Nothing
    PatOr p1 p2 ->
      case (m1, m2) of
        (Nothing, Nothing) -> Nothing
        (Just m1', Nothing) -> Just m1'
        (Nothing, Just m2') -> Just m2'
        (Just m1', Just m2') -> Just (m1' <> m2')
      where
        m1 = lineDefault (p1:ps) a
        m2 = lineDefault (p2:ps) a

defaultCM :: ClauseMatrix -> ClauseMatrix
defaultCM m =
  case m of
    (l : ls, a : as) ->
      case newLine of
        Nothing -> defaultCM (ls, as)
        Just x -> x <> defaultCM (ls, as)
      where
        newLine = lineDefault l a
    (_, _) -> ([], [])

-----------------------------------------------------------
-----------------------------------------------------------
-----------------------------------------------------------

{-
  Decision Trees
-}

type Occurrence = [Val]

data DecisionTree
  = Leaf Action
  | Fail
  | Switch Val [(Constr, DecisionTree)]
  | Swap Int DecisionTree
  deriving (Show, Eq, Ord)


allWild :: [Pat] -> Bool
allWild [] = True
allWild (PatWild:ps) = allWild ps
allWild (_:_) = False

swapLin :: Int -> [a] -> [a]
swapLin 0 l = l
swapLin n l = 
  if n >= length l then
    swapLin (n `mod` length l) l
  else 
    [l !! n] ++ drop 1 (take n l) ++ [head l] ++ drop (succ n) l

swapCol :: Int -> Matrix Pat -> Matrix Pat
swapCol 0 m = m
swapCol n m = transpose (swapLin n (transpose m))


findCol :: Matrix Pat -> Int
findCol [] = 1
findCol (l : ls) =
  if allWild l then
    1 + findCol ls
  else
    0


getConstr :: Pat -> [Constr]
getConstr PatWild = []
getConstr (PatConstr c _) = [c]
getConstr (PatOr p1 p2) = getConstr p1 ++ getConstr p2

collectHeadMatrix :: Matrix Pat -> [Constr]
collectHeadMatrix [] = []
collectHeadMatrix ([]:_) = []
collectHeadMatrix ((p:_):ls) = getConstr p ++ collectHeadMatrix ls


compilationScheme :: Ctx -> Occurrence -> ClauseMatrix -> DecisionTree
compilationScheme ctx (o:os) (l : ls, a : as) =
  case l of
    [] -> Leaf a
    _ ->
      if allWild l then
        Leaf a
      else
        if i == 0 then
          Switch o l'
        else
          Swap (i+1) (Switch o l')
        where
          i = findCol (transpose (l:ls))
          m' = swapCol i (l:ls)
          o' = swapLin i (o:os)
          
          x = sig s ctx

          s = nub (collectHeadMatrix m')
          mk = if x then map (flip spec (m', a:as)) s
               else map (flip spec (m', a:as)) s ++ [defaultCM (m', a:as)]
          s' = if x then s
               else s ++ [Constr "*" 0 "default"]

          ak = case o' of
                [] -> error "Isso não deveria acontecer"
                o'' : os' ->
                  case o'' of
                    ValConstr _ ps -> map (compilationScheme ctx (ps ++ os')) mk


          l' = zip s' ak
compilationScheme _ _ _ = Fail -- m = 0 e outras situações
