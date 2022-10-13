module Operations (
    normalizePolynomial
) where

import Types
import Sorters
import Accumulators
import Filters

normalizePolynomial :: Polynomial -> Polynomial
normalizePolynomial =  sortTerms . accumulatePolynomialPowers . sortPolynomialPowers . filterPolynomial
