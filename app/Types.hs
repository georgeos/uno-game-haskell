{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

data Color = R | G | B | Y deriving (Show, Eq, Read)

type Number = Int

data Card = Card
  { color :: Color
  , number :: Number
  } deriving (Eq)

instance Show Card where
  show :: Card -> String
  show c = show (color c) ++ show (number c)

fromCharToColor :: Char -> Maybe Color
fromCharToColor 'R' = Just R
fromCharToColor 'G' = Just G
fromCharToColor 'B' = Just B
fromCharToColor 'Y' = Just Y
fromCharToColor _   = Nothing

data User = User
  { pos       :: Number
  , userCards :: [Card]
  }

instance Show User where
  show :: User -> String
  show u = "User: " ++ show (pos u) ++ "\n" ++ "Cards: " ++ show (userCards u) ++ "\n"

type Users = [User]