module Parsers (
    Expr(..),
    parseTokens
) where

import Types
import Lexers

data Expr = PlusExpr Expr Expr
    | TimesExpr Expr Expr
    | PowerExpr Variable Number
    | VariableExpr Variable
    | LiteralExpr Number
    deriving (Show)

parseLiteralOrVariableOrPower :: [Token] -> Maybe (Expr, [Token])
parseLiteralOrVariableOrPower (VariableToken v : PowerToken : LiteralToken n : tks) = Just (PowerExpr v n, tks)
parseLiteralOrVariableOrPower (LiteralToken n1 : PowerToken : LiteralToken n2 : tks) = Just (LiteralExpr (n1 ^ n2), tks)
parseLiteralOrVariableOrPower (LiteralToken n : tks) = Just (LiteralExpr n, tks)
parseLiteralOrVariableOrPower (MinusToken : LiteralToken n : tks) = Just (LiteralExpr (-n), tks)
parseLiteralOrVariableOrPower (VariableToken v : tks) = Just (VariableExpr v, tks)
parseLiteralOrVariableOrPower (OpenParenthesisToken : restTokens1) =
    case parseSumOrProductOrLiteralOrVariableOrPower restTokens1 of
        Just (expr, CloseParenthesisToken : restTokens2) -> Just (expr, restTokens2)
        _                                                -> Nothing
parseLiteralOrVariableOrPower _ = Nothing

parseProductOrLiteralOrVariableOrPower :: [Token] -> Maybe (Expr, [Token])
parseProductOrLiteralOrVariableOrPower tokens =
    case parseLiteralOrVariableOrPower tokens of
        Just (expr1, TimesToken : restTokens1) ->
            case parseProductOrLiteralOrVariableOrPower restTokens1 of
                Just (expr2, restTokens2) -> Just (TimesExpr expr1 expr2, restTokens2)
                Nothing                   -> Nothing
        result -> result

parseSumOrProductOrLiteralOrVariableOrPower :: [Token] -> Maybe (Expr, [Token])
parseSumOrProductOrLiteralOrVariableOrPower tokens =
    case parseProductOrLiteralOrVariableOrPower tokens of
        Just (expr1, PlusToken : restTokens1) ->
            case parseSumOrProductOrLiteralOrVariableOrPower restTokens1 of
                Just (expr2, restTokens2) -> Just (PlusExpr expr1 expr2, restTokens2)
                Nothing                   -> Nothing
        result -> result

parseTokens :: [Token] -> Expr
parseTokens tokens =
    case parseSumOrProductOrLiteralOrVariableOrPower tokens of
        Just (expr, []) -> expr
        _               -> error "could not parse input"
