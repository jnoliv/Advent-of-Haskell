-- | This module contains helpers for solving linear equations using the Z3 solver.
module Advent.Math.LinearEquations (
    solve, solveAll
) where

import Control.Monad ((<=<), guard, zipWithM)
import Data.Maybe (catMaybes, fromMaybe)
import Z3.Monad

{- | Solve a system of linear integer equations, with the first argument being
the coefficient matrix and the second argument being the result vector. Returns
either a single solution, if the system has one or infinite solutions, or an
empty list, if the system is impossible to solve.

For example, to solve the following system of equations

@
94x + 22y = 8400,
34x + 67y = 5400.
@

one would write the following

>>> solve [[94,22],[34,67]] [8400,5400]
[80,40]

==== __Examples__
>>> solve [[1,1],[2,2]] [1,3]
[]
-}
solve :: Integral a
        => [[a]]        -- ^ Coefficient matrix (A)
        -> [a]          -- ^ Result vector (b)
        -> IO [Integer] -- ^ A solution vector to @Ax = b@ if one exists, the empty list otherwise
solve coeffs consts = evalZ3 $ do
    vars <- buildLinearEquations coeffs consts

    fromMaybe [] <$> getSolution vars

{- | Solve a system of linear integer equations, with the first argument being
the maximum number of solutions to return, the second argument being the
coefficient matrix and the third argument being the result vector. Returns
either a single solution, if the system has one or infinite solutions, or an
empty list, if the system is impossible to solve.

For example, to obtain 5 solutions of the following system of equations

@
 x +  y +  z ​= 3,
2x + 2y + 2z = 6,
3x + 3y + 3z = 9.​
@

>>> solveAll 5 [[1,1,1],[2,2,2],[3,3,3]] [3,6,9]
[[11,-4,-4],[9,-3,-3],[7,-2,-2],[5,-1,-1],[3,0,0]]
-}
solveAll :: Integral a
        => Int            -- ^ Maximum number of solutions to return
        -> [[a]]          -- ^ Coefficient matrix (A)
        -> [a]            -- ^ Result vector (b)
        -> IO [[Integer]] -- ^ A solution vector to @Ax = b@ if one exists, the empty list otherwise
solveAll max coeffs consts = evalZ3 do
    vars <- buildLinearEquations coeffs consts

    let 
        -- Get at most max solutions and return them in a list.
        getSolutions :: MonadZ3 m => [[Integer]] -> m [[Integer]]
        getSolutions acc
            | length acc >= max = return acc
            | otherwise = do
                maybeSolution <- getSolution vars
                case maybeSolution of
                    Nothing   -> return acc
                    Just sols -> do
                        restrictSolutions sols
                        getSolutions (sols : acc)
        
        -- Add Z3 constraints to avoid repeated solutions.
        restrictSolutions :: (Integral a, MonadZ3 m) => [a] -> m ()
        restrictSolutions sols = assert =<< mkNot =<< mkOr =<< zipWithM mkEq vars =<< mapM mkIntNum sols

    getSolutions []

-- Build the Z3 AST representing the given Ax = b system of equations.
buildLinearEquations :: (Integral a, MonadZ3 m) => [[a]] -> [a] -> m [AST]
buildLinearEquations coeffs consts = do
    varsAST   <- mapM (mkFreshIntVar . ("x" ++) . show . fst) . zip [1..] $ consts
    coeffsAST <- mapM (mapM mkIntNum) coeffs
    constsAST <- mapM mkIntNum consts

    lhssAST <- mapM (mkAdd <=< zipWithM (\a b -> mkMul [a,b]) varsAST) coeffsAST

    mapM_ assert =<< zipWithM mkEq constsAST lhssAST

    return varsAST

-- Get one solution for a Ax = b system of equations given as a list of Z3 ASTs. 
getSolution :: MonadZ3 m => [AST] -> m (Maybe [Integer])
getSolution vars = fmap snd <$> withModel $ \m -> catMaybes <$> mapM (evalInt m) vars
