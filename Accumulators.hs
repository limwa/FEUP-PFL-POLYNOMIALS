module Accumulators (
    accumulatePolynomialPowers,
    accumulatePolynomialTerms
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

accumulatePolynomialTerms :: Polynomial -> Polynomial
accumulatePolynomialTerms (Polynomial terms) = Polynomial (accumulatePolynomialCoefs terms)

accumulatePolynomialCoefs :: [Term] -> [Term]
accumulatePolynomialCoefs [] = []
accumulatePolynomialCoefs ((Term powers coef):ts) | newCoef == 0 = accumulatePolynomialCoefs otherPowers
                                                  | otherwise = Term powers newCoef : accumulatePolynomialCoefs otherPowers
    where (samePowers, otherPowers) = partition (\(Term powers' _) -> powers == powers') ts
          sameCoef = map (\(Term _ coef') -> coef') samePowers
          newCoef = coef + sum sameCoef

-- [2x⁵ + 5x⁷ + 8] * [3y²x³ + 7x⁴ + 1]