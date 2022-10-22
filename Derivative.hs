module Derive (
    derivativeOf
) where

import Types
import Normalize
import Data.List (partition)

-- To differentiate a polynomial, whose terms are all added up together, with respect to a variable, dvar,
-- we can find the derivative of each term with respect to dvar and add the results together.
derivativeOf :: Variable -> Polynomial -> Polynomial
derivativeOf dvar = normalize . derivativeOfTermsInPolynomial dvar . normalize
    where
        derivativeOfTermsInPolynomial :: Variable -> Polynomial -> Polynomial
        derivativeOfTermsInPolynomial dvar (Polynomial terms) = Polynomial (map (derivativeOfTerm dvar) terms)

-- To find the derivative of a term with respect to dvar, assuming there is only one power with dvar as its base,
-- we can subtract one to the exponent of the power whose variable is dvar and multiply the coeficient by the
-- original exponent
-- If there is no power with dvar as its base, the resulting coeficient for the term is 0, since the term is a
-- constant with respect to dvar
derivativeOfTerm :: Variable -> Term -> Term
derivativeOfTerm dvar (Term powers coef) = Term (Power dvar newExp : otherDvarPowers) newCoef
    where 
        (sameDvarPowers, otherDvarPowers) = partition (\(Power var _) -> var == dvar) powers
        sameDvarExp = (\(Power _ exp) -> exp) (head sameDvarPowers)
        newExp 
            | null sameDvarPowers = 0
            | otherwise = sameDvarExp - 1
        newCoef
            | null sameDvarPowers = 0
            | otherwise = coef * sameDvarExp
