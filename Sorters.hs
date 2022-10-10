module Sorters (
    sortPolynomialPowers
) where

import Types
import Data.List (sort)

sortPolynomialPowers :: Polynomial -> Polynomial
sortPolynomialPowers (Polynomial terms) = Polynomial (map sortTermPowers terms) 

sortTermPowers :: Term -> Term
sortTermPowers (Term powers coef) = Term (sort powers) coef
