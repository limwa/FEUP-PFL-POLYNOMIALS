module Types (
    Polynomial(..),
    Term(..),
    Power(..)
) where

type Variable = Char
type Number = Integer

data Power = Power Variable Number deriving (Show, Eq, Ord)
data Term = Term [Power] Number deriving (Show, Eq, Ord)
newtype Polynomial = Polynomial [Term] deriving (Show, Eq)
