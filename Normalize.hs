module Normalize (
    normalize,
) where

import Types
import Data.List (partition, sort)

normalize :: Polynomial -> Polynomial
normalize = sortTerms . addSamePowersTerms . sortPowers . multiplySameVarPowers . filterNullCoefsAndNullPowers

-- 1st step => remove all powers with 0 as their exponent and terms with 0 as their coeficient
-- This is done because powers with 0 as their exponent are equal to 1, which is the neutral element of multiplication
-- and because terms with 0 as their coeficient are equal to 0, which is the neutral element of addition
-- Therefore, removing these elements will not affect the final result
filterNullCoefsAndNullPowers :: Polynomial -> Polynomial
filterNullCoefsAndNullPowers = filterNullCoefs . filterNullPowers
          
filterNullCoefs :: Polynomial -> Polynomial
filterNullCoefs (Polynomial terms) = Polynomial notNullCoefs
    where 
        notNullCoefs = filter (\(Term powers coef) -> coef /= 0) terms

filterNullPowers :: Polynomial -> Polynomial
filterNullPowers (Polynomial terms) = Polynomial (map filterNullPowersFromTerms terms)

filterNullPowersFromTerms :: Term -> Term
filterNullPowersFromTerms (Term powers coef) = Term notNullPowers coef
    where
        notNullPowers = filter (\(Power var exp) -> exp /= 0) powers

-- 2nd step => for each term, add the powers with the same variable
-- This is done because all the powers in a term are all in a big multiplication
-- Therefore, if the powers have the same base (a.k.a. variable), their exponents can be added together
multiplySameVarPowers :: Polynomial -> Polynomial
multiplySameVarPowers (Polynomial terms) = Polynomial (map multiplySameVarPowersInTerms terms)

-- The way we multiply the powers in a given term is by applying this recursive strategy to the powers of a term:
-- 1. Get all the powers in the list with the same base (or variable) as the first one in the list
-- 2. Add the exponents of those powers together
-- 3. Apply the same process to the powers that were not selected in 1.
-- 4. If the result obtained in 2. was not 0, we add a power with the new exponent to the result obtained in 3.
multiplySameVarPowersInTerms :: Term -> Term
multiplySameVarPowersInTerms (Term powers coef) = Term (mergePowers powers) coef 
    where
        mergePowers :: [Power] -> [Power]
        mergePowers [] = []
        mergePowers ((Power var exp):xs)
            | newExp == 0 = mergePowers otherVarPowers
            | otherwise = Power var newExp : mergePowers otherVarPowers
            where 
                (sameVarPowers, otherVarPowers) = partition (\(Power var' exp') -> var == var') xs
                sameVarExponents = map (\(Power _ exp') -> exp') sameVarPowers
                newExp = exp + sum sameVarExponents

-- 3rd step => sort all the powers in each term
-- The order with which we sort the powers is not relevant, as long as it is the same for every term
-- This is done so that, in the next step, we can compare the powers list with the equals operator (==)
-- and to avoid sorting those lists multiple times in the next step
sortPowers :: Polynomial -> Polynomial
sortPowers (Polynomial terms) = Polynomial (map sortPowersInTerms terms)
    where 
        sortPowersInTerms :: Term -> Term
        sortPowersInTerms (Term powers coef) = Term (sort powers) coef

-- 4th step => add the coeficients of the terms with the same powers
-- This is done because when two terms with the same powers are added together,
-- the result is a new term where the powers are the same, but the coeficients
-- are added together.

-- The way we add the terms in a polynomial is by applying this recursive strategy to the list of terms of a polynomial:
-- 1. Get all the terms in the list with the same powers as the first one in the list
-- 2. Add the coeficients of those terms together
-- 3. Apply the same process to the terms that were not selected in 1.
-- 4. If the result obtained in 2. was not 0, we add a term with the new coeficient to the result obtained in 3.
addSamePowersTerms :: Polynomial -> Polynomial
addSamePowersTerms (Polynomial terms) = Polynomial (mergeTerms terms)
    where
        mergeTerms :: [Term] -> [Term]
        mergeTerms [] = []
        mergeTerms ((Term powers coef):ts)
            | newCoef == 0 = mergeTerms otherPowers
            | otherwise = Term powers newCoef : mergeTerms otherPowers
            where 
                (samePowers, otherPowers) = partition (\(Term powers' _) -> powers == powers') ts
                sameCoef = map (\(Term _ coef') -> coef') samePowers
                newCoef = coef + sum sameCoef


-- 5th step => sort the terms
-- This is mostly done in order to more easily compare polynomials
sortTerms :: Polynomial -> Polynomial
sortTerms (Polynomial terms) = Polynomial (reverse (sort terms))