module Operations (
    normalizePolynomial,
    multiplyPolynomials,
    derivePolynomial
) where

import Types
import Sorters
import Accumulators
import Filters
import Data.List

normalizePolynomial :: Polynomial -> Polynomial
normalizePolynomial = accumulatePolynomialTerms . sortTerms . accumulatePolynomialPowers . sortPolynomialPowers . filterPolynomial

sumPolynomial :: [Polynomial] -> Polynomial
sumPolynomial [] = Polynomial []
sumPolynomial (Polynomial terms:ps) = normalizePolynomial (Polynomial (terms ++ newTerms))
    where (Polynomial newTerms) = sumPolynomial ps

multiplyPolynomials :: Polynomial -> Polynomial -> Polynomial
multiplyPolynomials (Polynomial terms) (Polynomial terms') = normalizePolynomial (Polynomial [multiplyTerms t t' | t <- terms, t' <- terms'])
    where multiplyTerms (Term powers coef) (Term powers' coef') = Term (powers ++ powers') (coef * coef')

derivePolynomial :: Variable -> Polynomial -> Polynomial
derivePolynomial dvar = normalizePolynomial . helper . normalizePolynomial
    where helper (Polynomial terms) = Polynomial (map (deriveTerm dvar) terms)

deriveTerm :: Variable -> Term -> Term
deriveTerm dvar (Term powers coef) = Term (Power dvar newExp : otherDvarPowers) newCoef
    where (sameDvarPowers, otherDvarPowers) = partition (\(Power var _) -> var == dvar) powers
          newExp | not (null sameDvarPowers) = (\(Power _ exp) -> exp) (head sameDvarPowers) - 1
                 | otherwise = 0
          newCoef | null sameDvarPowers = 0
                  | otherwise = coef * (newExp + 1)

deriveTerm' :: Variable -> Term ->  Term
deriveTerm' dvar (Term powers coef) = Term (newSameDvarPowers ++ otherDvarPowers) newCoef
    where (sameDvarPowers, otherDvarPowers) = partition (\(Power var _) -> var == dvar) powers
          newSameDvarPowers = [Power dvar (exp - 1) | (Power _ exp) <- sameDvarPowers ]
          newCoef | null sameDvarPowers = 0
                  | otherwise = coef * sum [exp | (Power _ exp) <- sameDvarPowers]
