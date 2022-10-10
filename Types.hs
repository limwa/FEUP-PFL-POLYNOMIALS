module Types (
    Polynomial(..),
    Term(..),
    Power(..)
) where

type Variable = Char
type Number = Integer

data Power = Power Variable Number deriving (Show, Eq)
data Term = Term Number [Power] deriving (Show, Eq)
newtype Polynomial = Polynomial [Term] deriving (Show, Eq)
