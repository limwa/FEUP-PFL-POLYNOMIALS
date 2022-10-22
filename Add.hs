module Add (
    add,
) where

import Types
import Normalize

-- Since addition is associative, the sum of two polynomials
-- whose terms are all added together, is equal to another polynomial
-- that has all the terms of both polynomials
-- After adding both polynomials together, we normalize the resulting
-- polynomial so that the addition of the terms is executed.
add :: Polynomial -> Polynomial -> Polynomial
add (Polynomial terms) (Polynomial terms') = normalize (Polynomial (terms ++ terms'))
