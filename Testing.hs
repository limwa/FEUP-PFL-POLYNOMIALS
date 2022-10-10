import Types
import Filters
import Accumulators
import Sorters

-- 7x^2 + 0x^3 + 3x^3 + 12x^0 + 4x^2 + 7x^2x^3 + 8x^2 + 1x^3 + 12x^2 x^1 y^2 x^0
testPoly = Polynomial [ Term [ Power 'x' 2 ] 7, Term [ Power 'x' 3 ] 0, Term [ Power 'x' 3 ] 3, Term [ Power 'x' 0 ] 12, Term [ Power 'x' 2 ] 4, Term [ Power 'x' 2, Power 'x' 3 ] 7, Term [ Power 'x' 2 ] 8, Term [ Power 'x' 3 ] 1, Term [ Power 'x' 2, Power 'x' 1, Power 'y' 2, Power 'x' 0 ] 12 ]

main = print ((accumulatePolynomialPowers . sortPolynomialPowers . filterPolynomial) testPoly)