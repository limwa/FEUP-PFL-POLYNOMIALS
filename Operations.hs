module Operations (
    normalizePolynomial
) where

import Types
import Sorters
import Accumulators
import Filters

normalizePolynomial :: Polynomial -> Polynomial
normalizePolynomial = accumulatePolynomialTerms . sortTerms . accumulatePolynomialPowers . sortPolynomialPowers . filterPolynomial

sumPolynomial :: [Polynomial] -> Polynomial
sumPolynomial [] = Polynomial []
sumPolynomial (Polynomial terms:ps) = normalizePolynomial (Polynomial (terms ++ newTerms))
    where (Polynomial newTerms) = sumPolynomial ps
