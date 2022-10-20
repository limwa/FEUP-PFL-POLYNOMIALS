module Lexers (
    Token(..),
    lexer
) where

import Types (Variable, Number)
import Data.Char

data Token = PlusToken
    | MinusToken
    | TimesToken
    | PowerToken
    | OpenParenthesisToken
    | CloseParenthesisToken
    | VariableToken Variable
    | LiteralToken Number
    deriving (Show, Eq)

lexer :: String -> [Token]
lexer "" = []

lexer str
    | isDigit (head str) = LiteralToken (readLiteral digits) : lexer rest
    where
        (digits, rest) = span isDigit str
        readLiteral :: String -> Int
        readLiteral = foldl (\acc c -> 10 * acc + digitToInt c) 0

lexer (c:cs)
    | isSpace c = lexer cs
    | c == '+' = PlusToken : lexer cs
    | c == '-' = MinusToken : lexer cs
    | c == '*' = TimesToken : lexer cs
    | c == '^' = PowerToken : lexer cs
    | c == '(' = OpenParenthesisToken : lexer cs
    | c == ')' = CloseParenthesisToken : lexer cs
    | isAlpha c = VariableToken c : lexer cs
    | otherwise = error "unsupported character"
