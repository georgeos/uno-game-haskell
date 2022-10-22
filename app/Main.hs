module Main where

import Control.Monad (replicateM)
import Data.Function (on)
import Data.List     (sortBy)
import qualified System.Random as R
import           Text.Read (readMaybe)
import            Types

randomize :: [Card] -> [Int] -> [Card]
randomize xs ys = map fst $ sortBy (compare `on` snd) (zip xs ys)

cards :: [Card]
cards = [
  Card {
      color = x
    , number = y
  } | x <- [R, G, B, Y], y <- [0..9] ]

cardValid :: Card -> Card -> Bool
cardValid c1 c2 =
  color  c1 == color  c2 ||
  number c1 == number c2

main :: IO ()
main = do
  putStrLn "Enter number of players (2-5): "
  mn <- getNumberPlayer
  case mn of
    Nothing -> putStrLn "Enter a valid number of players" >> main
    Just n  -> if n >= 2 && n <= 5
      then
        do
          randomList <- replicateM (length cards) (R.randomRIO (1 :: Int, 100000))
          print $ ditributeCards (randomize cards randomList) n
      else putStrLn "Invalid number of players" >> main
  where
    getNumberPlayer :: IO (Maybe Int)
    getNumberPlayer = do readMaybe <$> getLine

ditributeCards :: [Card] -> Int -> [User]
ditributeCards cards = generateUserCards 0
  where
    generateUserCards :: Int -> Int -> [User]
    generateUserCards n t
      | n < t =
          User {
              pos = n
            , userCards = drop (4 * n) $ assignCards n cards
          } : generateUserCards (n + 1) t
      | otherwise = []
    assignCards :: Int -> [Card] -> [Card]
    assignCards n = take $ 4 * (n+1)
