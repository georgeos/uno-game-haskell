{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

data Color = RED | GREEN | BLUE | YELLOW deriving (Show, Eq)

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
  }

instance Show User where
  show :: User -> String
  show u = "User: " ++ show (pos u) ++ "\n" ++ "Cards: " ++ show (userCards u) ++ "\n"

type Users = [User]