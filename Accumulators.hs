module Accumulators (
    accumulatePolynomialPowers
) where

import Types
import Data.List (partition)

accumulatePolynomialPowers :: Polynomial -> Polynomial
accumulatePolynomialPowers (Polynomial terms) = Polynomial (map accumulateTermPowers terms)

accumulateTermPowers :: Term -> Term
accumulateTermPowers (Term powers coef) = Term (accumulatePowers powers) coef 

accumulatePowers :: [Power] -> [Power]
accumulatePowers [] = []
accumulatePowers ((Power var exp):xs) | newExp == 0 = accumulatePowers otherVarPowers
                                    | otherwise = Power var newExp : accumulatePowers otherVarPowers
    where (sameVarPowers, otherVarPowers) = partition (\(Power var' exp') -> var == var') xs
          sameVarExponents = map (\(Power _ exp') -> exp') sameVarPowers
          newExp = exp + sum sameVarExponents

accumulatePolynomialCoefs :: [Term] -> [Term]
accumulatePolynomialCoefs [] = []
accumulatePolynomialCoefs ((Term powers coef):ts) = ts
    where (samePowers, otherPowers) = partition (\(Term powers' coef) -> powers == powers') ts
          addCoefs = Term(power sum coef) samePowers
-- accumulatePolynomialCoefs (Polynomial terms) | 

fgdo :: Polynomial -> [Integer]
fgdo (Polynomial [x]) = [9]
fgdo (Polynomial (n:(Term powers coef):ns)) = [coef]
-- fgdo nums = nums


