{-# LANGUAGE InstanceSigs #-}

module Types where

data Color = R | G | B | Y deriving (Show, Eq)

type Number = Int

data Card = Card
  { color :: Color
  , number :: Number
  }

instance Show Card where
  show :: Card -> String
  show c = show (color c) ++ show (number c)

data User = User
  { pos       :: Number
  , userCards :: [Card]
  } deriving (Show)
