module Operations (
    normalizePolynomial
) where

import Types
import Sorters (sortPolynomialPowers)
import Accumulators (accumulatePolynomialPowers)
import Filters (filterPolynomial)

normalizePolynomial :: Polynomial -> Polynomial
normalizePolynomial = sortPolynomialPowers . accumulatePolynomialPowers . filterPolynomial