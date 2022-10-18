module Lexers (
    Token(..)
) where

import Types (Variable, Number)
import Data.Char

data Token = PlusToken
    | TimesToken
    | PowerToken
    | OpenParenthesisToken
    | CloseParenthesisToken
    | VariableToken Variable
    | LiteralToken Number
    deriving (Show)

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
    | c == '*' = TimesToken : lexer cs
    | c == '^' = PowerToken : lexer cs
    | c == '(' = OpenParenthesisToken : lexer cs
    | c == ')' = CloseParenthesisToken : lexer cs
    | isAlpha c = VariableToken c : lexer cs
    | otherwise = error "unsupported character"


main = print (lexer "7 * x * x^3 + y ^ 2 + 394 * x^(100 + 200)")