module Parsers (
    Expr(..),
    parse
) where

import Types
import Lexer
import Add
import Multiply
import Normalize (normalize)

data Expr = PlusExpr Expr Expr
    | NegExpr Expr
    | TimesExpr Expr Expr
    | PowerExpr Expr Number
    | VariableExpr Variable
    | LiteralExpr Number
    | ParenthesisedExpr Expr
    deriving (Show)

-- The parser parses an expression recursively
-- Firstly, it will try to parse numbers, variables or expressions in parenthesis
-- Secondly, it will try to parse powers
-- Thirdly, it will try to parse as many multiplications as it can before returning
-- In fourth place, it will try to parse as many additions or subtractions as it can before return
--
-- The parsers return Nothing if the expression can't be parsed so that those cases can be better
-- handled (and, in those cases, trying to parse lower priority operations)

-- Parses numbers, variables and parenthesis. Also handles the special case of when a number starts with "-"
parseLiteralOrVariable :: [Token] -> Maybe (Expr, [Token])
parseLiteralOrVariable (LiteralToken n : tks) = Just (LiteralExpr n, tks)
parseLiteralOrVariable (VariableToken v : tks) = Just (VariableExpr v, tks)
parseLiteralOrVariable (MinusToken : tks) = parseSumOrProductOrPowerLiteralOrVariable (LiteralToken 0 : MinusToken : tks)
parseLiteralOrVariable (OpenParenthesisToken : restTokens1) =
    case parseSumOrProductOrPowerLiteralOrVariable restTokens1 of
        Just (expr, CloseParenthesisToken : restTokens2) -> Just (ParenthesisedExpr expr, restTokens2)
        _                                                -> Nothing
parseLiteralOrVariable _ = Nothing

-- Parses powers and all of the above mentioned tokens
parsePowerOrLiteralOrVariable :: [Token] -> Maybe (Expr, [Token])
parsePowerOrLiteralOrVariable tokens = 
    case parseLiteralOrVariable tokens of
        Just (expr1, PowerToken : LiteralToken exp : restTokens1) -> Just (PowerExpr expr1 exp, restTokens1)
        result -> result

-- Parses products as well as the above mentioned tokens
parseProductOrPowerOrLiteralOrVariable :: [Token] -> Maybe (Expr, [Token])
parseProductOrPowerOrLiteralOrVariable tokens =
    case parsePowerOrLiteralOrVariable tokens of
        Just (expr1, TimesToken : restTokens1) ->
            case parseProductOrPowerOrLiteralOrVariable restTokens1 of
                Just (expr2, restTokens2) -> Just (TimesExpr expr1 expr2, restTokens2)
                _                         -> Nothing
        result -> result

-- Parses additions as well as the above mentioned tokens
parseSumOrProductOrPowerLiteralOrVariable :: [Token] -> Maybe (Expr, [Token])
parseSumOrProductOrPowerLiteralOrVariable tokens =
    case parseProductOrPowerOrLiteralOrVariable tokens of
        Just (expr1, PlusToken : restTokens1) ->
            case parseSumOrProductOrPowerLiteralOrVariable restTokens1 of
                Just (expr2, restTokens2) -> Just (PlusExpr expr1 expr2, restTokens2)
                _                         -> Nothing

        -- Parsing a minus is a special case because it has the same priority as the additions
        -- and we need to store the negative sign without affecting the expressions that follow
        -- That's why we have a special transformation for the case that what follows is an addition
        Just (expr1, MinusToken : restTokens1) ->
            case parseSumOrProductOrPowerLiteralOrVariable restTokens1 of
                Just (PlusExpr expr2 expr3, restTokens2) -> Just (PlusExpr expr1 (PlusExpr (NegExpr expr2) expr3), restTokens2)
                Just (expr2, restTokens2) -> Just (PlusExpr expr1 (NegExpr expr2), restTokens2)
                _                                        -> Nothing
        result -> result

-- Tries to parse the list of tokens
-- If the expression can't be parsed, an error is thrown
parseTokens :: [Token] -> Expr
parseTokens tokens =
    case parseSumOrProductOrPowerLiteralOrVariable tokens of
        Just (expr, []) -> expr
        _               -> error "could not parse input"

-- Recursively transforms an expression to a polynomial
polynomialFromExpr :: Expr -> Polynomial
polynomialFromExpr (PlusExpr expr expr') = add (polynomialFromExpr expr) (polynomialFromExpr expr')
polynomialFromExpr (NegExpr expr) = Polynomial (mapTerms terms)
    where 
        (Polynomial terms) = polynomialFromExpr expr
        mapTerms = map (\(Term powers coef) -> Term powers (-coef))

polynomialFromExpr (TimesExpr expr expr') = multiply (polynomialFromExpr expr) (polynomialFromExpr expr')
polynomialFromExpr (PowerExpr expr exp) = foldr multiply (Polynomial [ Term [] 1 ]) (replicate exp (polynomialFromExpr expr))
polynomialFromExpr (VariableExpr var) = Polynomial [ Term [ Power var 1 ] 1 ]
polynomialFromExpr (LiteralExpr num) = Polynomial [ Term [] num ]
polynomialFromExpr (ParenthesisedExpr expr) = polynomialFromExpr expr

-- Parses and normalizes a polynomial from a string
parse :: String -> Polynomial
parse input = normalize (polynomialFromExpr expr)
    where expr = parseTokens (lexer input)
