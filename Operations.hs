module Operations (
    normalizePolynomial,
    multiplyPolynomials,
    derivePolynomial
) where

import Types
import Sorters
import Accumulators
import Filters
import Parsers
import Lexers
import Data.List

normalizePolynomial :: Polynomial -> Polynomial
normalizePolynomial = accumulatePolynomialTerms . sortTerms . accumulatePolynomialPowers . sortPolynomialPowers . filterPolynomial

sumPolynomials :: Polynomial -> Polynomial -> Polynomial
sumPolynomials (Polynomial terms1) (Polynomial terms2) = normalizePolynomial (Polynomial (terms1 ++ terms2))

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

polynomialFromExpr :: Expr -> Polynomial
polynomialFromExpr (PlusExpr expr1 expr2) = sumPolynomials (polynomialFromExpr expr1) (polynomialFromExpr expr2)
polynomialFromExpr (NegExpr expr) = Polynomial (mapTerms terms)
    where (Polynomial terms) = polynomialFromExpr expr
          mapTerms = map (\(Term powers coef) -> Term powers (-coef))
polynomialFromExpr (TimesExpr expr1 expr2) = multiplyPolynomials (polynomialFromExpr expr1) (polynomialFromExpr expr2)
polynomialFromExpr (PowerExpr expr exp) = foldr multiplyPolynomials (Polynomial [ Term [] 1 ]) (replicate exp (polynomialFromExpr expr))
polynomialFromExpr (VariableExpr var) = Polynomial [ Term [ Power var 1 ] 1 ]
polynomialFromExpr (LiteralExpr num) = Polynomial [ Term [] num ]
polynomialFromExpr (ParenthesisedExpr expr) = polynomialFromExpr expr

parse :: String -> Polynomial
parse input = normalizePolynomial (polynomialFromExpr expr)
    where expr = parseTokens (lexer input)