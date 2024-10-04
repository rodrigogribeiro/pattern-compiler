{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore #-}

import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import System.Random
import qualified Data.Map as M
import Compiler

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "Exemplo merge" $
      compilationScheme (M.fromList [("List", [exConstr1, exConstr2]), ("Unit", [exConstr3])]) [exVal2, exVal2] exClause1 
      `compare` 
      Switch (ValConstr (Constr {name = "cons", arity = 2, ty = "List"}) [ValConstr (Constr {name = "Unit", arity = 0, ty = "Unit"}) [],ValConstr (Constr {name = "nil", arity = 0, ty = "List"}) []]) [(Constr {name = "nil", arity = 0, ty = "List"},Leaf 1),(Constr {name = "cons", arity = 2, ty = "List"},Swap 3 (Switch (ValConstr (Constr {name = "Unit", arity = 0, ty = "Unit"}) []) [(Constr {name = "nil", arity = 0, ty = "List"},Leaf 2),(Constr {name = "cons", arity = 2, ty = "List"},Leaf 3)]))]
      @?= EQ
  ]

-- main = do
--   n <- fst (randomR (1, 5) (mkStdGen 123) :: (IO Int, StdGen))
--   putStr (show (newCtx n))

maxNameSize :: Int
maxNameSize = 10

maxArity :: Int
maxArity = 7

newConstr :: String -> Constr
newConstr tyName = Constr name arity tyName
  where
    nameSize = fst (randomR (3, maxNameSize) (mkStdGen 123) :: (Int, StdGen))
    name = take nameSize $ randomRs ('a','z') (mkStdGen 123)
    arity = fst (randomR (1, maxArity) (mkStdGen 123) :: (Int, StdGen))

newTy :: Int -> [Constr]
newTy maxConstr = replicate numConstr (newConstr name)
  where
    nameSize = fst (randomR (3, maxNameSize) (mkStdGen 123) :: (Int, StdGen))
    name = take nameSize $ randomRs ('a','z') (mkStdGen 123)
    numConstr = fst (randomR (1, maxArity) (mkStdGen 123) :: (Int, StdGen))

newCtx :: Int -> Ctx
newCtx n = M.fromList [] -- (replicate n (newTy 10))
