module Types (
    Polynomial(..),
    Term(..),
    Power(..),
    Variable(..),
    Number(..)
) where

import Data.List (intercalate)

type Variable = Char
type Number = Integer

data Power = Power Variable Number deriving (Eq, Ord) -- x 2
data Term = Term [Power] Number deriving (Eq, Ord) -- [x 2, y 5] 6
newtype Polynomial = Polynomial [Term] deriving (Eq) -- [(y 4, x 5, 6), (y 8, 1), (y 4, -2)]

instance Show Power where
    show (Power var exp) = concat [[var], "^", show exp]

instance Show Term where
    show (Term powers coef) = show coef ++ (if null powers then "" else " " ++ powersStr)
        where powersStr = unwords (map show powers)

instance Show Polynomial where
    show (Polynomial terms) = intercalate " + " (map show terms)