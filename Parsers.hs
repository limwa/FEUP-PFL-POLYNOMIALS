module Parsers (
    Expr(..),
    parseTokens
) where

import Types
import Lexers

data Expr = PlusExpr Expr Expr
    | NegExpr Expr
    | TimesExpr Expr Expr
    | PowerExpr Expr Number
    | VariableExpr Variable
    | LiteralExpr Number
    | ParenthesisedExpr Expr
    deriving (Show)

parseLiteralOrVariable :: [Token] -> Maybe (Expr, [Token])
parseLiteralOrVariable (LiteralToken n : tks) = Just (LiteralExpr n, tks)
parseLiteralOrVariable (VariableToken v : tks) = Just (VariableExpr v, tks)

parseLiteralOrVariable (MinusToken : tks) = parseSumOrProductOrPowerLiteralOrVariable (LiteralToken 0 : MinusToken : tks)

parseLiteralOrVariable (OpenParenthesisToken : restTokens1) =
    case parseSumOrProductOrPowerLiteralOrVariable restTokens1 of
        Just (expr, CloseParenthesisToken : restTokens2) -> Just (ParenthesisedExpr expr, restTokens2)
        _                                                -> Nothing
parseLiteralOrVariable _ = Nothing

parsePowerOrLiteralOrVariable :: [Token] -> Maybe (Expr, [Token])
parsePowerOrLiteralOrVariable tokens = 
    case parseLiteralOrVariable tokens of
        Just (expr1, PowerToken : LiteralToken exp : restTokens1) -> Just (PowerExpr expr1 exp, restTokens1)
        result -> result

parseProductOrPowerOrLiteralOrVariable :: [Token] -> Maybe (Expr, [Token])
parseProductOrPowerOrLiteralOrVariable tokens =
    case parsePowerOrLiteralOrVariable tokens of
        Just (expr1, TimesToken : restTokens1) ->
            case parseProductOrPowerOrLiteralOrVariable restTokens1 of
                Just (expr2, restTokens2) -> Just (TimesExpr expr1 expr2, restTokens2)
                _                         -> Nothing
        result -> result

parseSumOrProductOrPowerLiteralOrVariable :: [Token] -> Maybe (Expr, [Token])
parseSumOrProductOrPowerLiteralOrVariable tokens =
    case parseProductOrPowerOrLiteralOrVariable tokens of
        Just (expr1, PlusToken : restTokens1) ->
            case parseSumOrProductOrPowerLiteralOrVariable restTokens1 of
                Just (expr2, restTokens2) -> Just (PlusExpr expr1 expr2, restTokens2)
                _                         -> Nothing
        Just (expr1, MinusToken : restTokens1) ->
            case parseSumOrProductOrPowerLiteralOrVariable restTokens1 of
                Just (PlusExpr expr2 expr3, restTokens2) -> Just (PlusExpr expr1 (PlusExpr (NegExpr expr2) expr3), restTokens2)
                Just (expr2, restTokens2) -> Just (PlusExpr expr1 (NegExpr expr2), restTokens2)
                _                                        -> Nothing
        result -> result

parseTokens :: [Token] -> Expr
parseTokens tokens =
    case parseSumOrProductOrPowerLiteralOrVariable tokens of
        Just (expr, []) -> expr
        _               -> error "could not parse input"
    where hasTwoMinuses = any (\(t1, t2) -> t1 == MinusToken && t2 == MinusToken) (zip tokens (tail tokens))
    
