module Multiply (
    multiply,
) where

import Types
import Normalize

-- To multiply two polynomials whose terms are all added together, we need to
-- apply the distributive rule to their terms, meaning that for each term in the first
-- polynomial, we need to multiply it with all terms in the second polynomial
-- To multiply two terms together, we multiply both terms' powers (concatenate the lists representing them)
-- and we multiply the coeficients
-- In the end, we normalize the result to simplify the expression
multiply :: Polynomial -> Polynomial -> Polynomial
multiply (Polynomial terms) (Polynomial terms') = normalize (Polynomial [multiplyTerms t t' | t <- terms, t' <- terms'])
    where 
        multiplyTerms :: Term -> Term -> Term
        multiplyTerms (Term powers coef) (Term powers' coef') = Term (powers ++ powers') (coef * coef')
