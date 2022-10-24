module Main where

import Control.Monad (replicateM)
import Data.Function (on)
import Data.List     (sortBy, elemIndex)
import qualified System.Random as R
import           Text.Read (readMaybe)
import            Types
import            Constants
import            Game

randomize :: [Card] -> [Int] -> [Card]
randomize xs ys = map fst $ sortBy (compare `on` snd) (zip xs ys)

cards :: Difficulty -> [Card]
cards d = [
  Card {
      color = x
    , number = y
  } | x <- [R, G, B, Y], y <- [0..a]]
  where
    a :: Int
    a | d == Low    = 4
      | d == Medium = 7
      | otherwise   = 9

introduction :: IO ()
introduction = do
  putStrLn "--------------------------------------------------------"
  putStrLn "WELCOME TO UNO GAME"
  putStrLn "--------------------------------------------------------"
  putStrLn ""
  putStrLn "Instructions:"
  putStrLn "- Every player has a set of cards"
  putStrLn "- Every player must play a similar card (color or number) to the previous played card"
  putStrLn "- If the player doesn't have a similar card, then must take one card from the main set"
  putStrLn "- First player without having cards, WINS!"
  putStrLn ""
  putStrLn "Lets play!"
  putStrLn ""

main :: IO ()
main = do
  introduction
  d <- getDifficulty
  putStrLn ""
  mn <- getNumberPlayer
  case mn of
    Nothing -> putStrLn "Enter a valid number of players" >> main
    Just n  -> if n >= minimumPlayers && n <= maximumPlayers
      then
        do
          randomList <- replicateM (length $ cards d) (R.randomRIO (1 :: Int, 100000))
          let randomCards = randomize (cards d) randomList
          let users = ditributeCards randomCards n
          let unusedCards = drop (n * cardsByPlayer) randomCards
          startGame unusedCards [] users
      else putStrLn "Invalid number of players" >> main
  where
    getDifficulty :: IO Difficulty
    getDifficulty = do
      putStrLn "Select the difficulty: "
      putStrLn "1) Low"
      putStrLn "2) Medium"
      putStrLn "3) High"
      fromCharToDifficulty . head <$> getLine

    getNumberPlayer :: IO (Maybe Int)
    getNumberPlayer = do
      putStrLn "Enter number of players (2-5): "
      readMaybe <$> getLine

    usedCards :: Users -> [Card]
    usedCards (u : us) = userCards u ++ usedCards us
    usedCards _ = []

ditributeCards :: [Card] -> Int -> Users
ditributeCards cards = generateUser 0
  where
    generateUser :: Int -> Int -> Users
    generateUser n t
      | n < t =
          User {
              pos = n
            , userCards = drop (cardsByPlayer * n) $ assignCards n cards
          } : generateUser (n + 1) t
      | otherwise = []
    assignCards :: Int -> [Card] -> [Card]
    assignCards n = take $ cardsByPlayer * (n + 1)

