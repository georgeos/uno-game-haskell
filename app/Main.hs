module Main where

import Control.Monad (replicateM)
import Data.Function (on)
import Data.List     (sortBy, elemIndex)
import qualified System.Random as R
import           Text.Read (readMaybe)
import            Types
import            Constants
import            Game

main :: IO ()
main = do

  introduction
  d <- getDifficulty
  n <- getNumberPlayers

  randomList <- replicateM (length $ cards d) (R.randomRIO (1 :: Int, 1000))
  let randomCards = randomize d randomList
      unusedCards = drop (n * cardsByPlayer) randomCards
      users = ditributeCards randomCards n
  startGame unusedCards [] users

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

getDifficulty :: IO Difficulty
getDifficulty = do
  putStrLn ""
  putStrLn "Select the difficulty: "
  putStrLn "1) Low"
  putStrLn "2) Medium"
  putStrLn "3) High"
  difficulty <- readMaybe <$> getLine

  case difficulty of
    Nothing -> putStrLn "Invalid difficulty" >> getDifficulty
    Just d
      | d >= 1 && d <= 3 -> pure $ fromIntToDifficulty d
      | otherwise -> putStrLn "Invalid difficulty" >> getDifficulty

getNumberPlayers :: IO Int
getNumberPlayers = do
  putStrLn ""
  putStrLn "Enter number of players (2-5): "
  players <- readMaybe <$> getLine

  case players of
    Nothing -> putStrLn "Enter a valid number of players" >> getNumberPlayers
    Just n  ->
      if n >= minimumPlayers && n <= maximumPlayers
        then return n
        else putStrLn "Invalid number of players" >> getNumberPlayers

randomize :: Difficulty -> [Int] -> [Card]
randomize d randomList = map fst $ sortBy (compare `on` snd) (zip (cards d) randomList)

cards :: Difficulty -> [Card]
cards d = [Card { color = x, number = y} | x <- [R, G, B, Y], y <- [0..a]]
  where
    a :: Int
    a | d == Low    = 4
      | d == Medium = 7
      | otherwise   = 9

ditributeCards :: [Card] -> Int -> Users
ditributeCards cards = generateUser 0
  where
    generateUser :: Int -> Int -> Users
    generateUser n t
      | n < t = User { pos = n, userCards = drop (cardsByPlayer * n) $ assignCards n cards } : generateUser (n + 1) t
      | otherwise = []

    assignCards :: Int -> [Card] -> [Card]
    assignCards n = take $ cardsByPlayer * (n + 1)