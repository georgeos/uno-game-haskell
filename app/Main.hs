module Main where

import Control.Monad (replicateM)
import Data.Function (on)
import Data.List     (sortBy, elemIndex)
import qualified System.Random as R
import           Text.Read (readMaybe)
import Control.Lens.Operators ((.~), (&))
import Control.Lens (element)
import            Types
import            Constants

randomize :: [Card] -> [Int] -> [Card]
randomize xs ys = map fst $ sortBy (compare `on` snd) (zip xs ys)

cards :: [Card]
cards = [
  Card {
      color = x
    , number = y
  } | x <- [R, G, B, Y], y <- [0..9] ]

validCard :: Maybe Card -> Card -> Bool
validCard Nothing c2 = True
validCard (Just c1) c2 =
  color  c1 == color  c2 ||
  number c1 == number c2

main :: IO ()
main = do
  putStrLn "Enter number of players (2-5): "
  mn <- getNumberPlayer
  case mn of
    Nothing -> putStrLn "Enter a valid number of players" >> main
    Just n  -> if n >= minimumPlayers && n <= maximumPlayers
      then
        do
          randomList <- replicateM (length cards) (R.randomRIO (1 :: Int, 100000))
          let randomCards = randomize cards randomList
          let users = ditributeCards randomCards n
          let unusedCards = drop (n * cardsByPlayer) randomCards
          startGame unusedCards [] users
      else putStrLn "Invalid number of players" >> main
  where
    getNumberPlayer :: IO (Maybe Int)
    getNumberPlayer = do readMaybe <$> getLine

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

startGame :: [Card] -> [Card] -> Users -> IO ()
startGame unusedCards playedCards users = do
  putStrLn "--------------------------------------------------------"
  putStrLn "Start of game!"
  putStrLn "--------------------------------------------------------"
  userPlay unusedCards playedCards users 0

getPosition :: Int -> Int -> Int
getPosition c t = if c < t - 1 then c + 1 else 0

lastCardPlayed :: [Card] -> Maybe Card
lastCardPlayed [] = Nothing
lastCardPlayed (c : cs) = Just c

userPlay :: [Card] -> [Card] -> Users -> Int -> IO ()
userPlay unusedCards playedCards users pos = do
  putStrLn ""
  putStrLn $ "User " ++ show pos
  -- putStrLn $ "Unused cards: " ++ show unusedCards
  putStrLn $ "Played cards: " ++ show playedCards
  let currentUser = users !! pos
  putStr "Your cards: "
  print $ userCards currentUser
  putStrLn "Enter your card to play or T to take one card or Q to quit: "
  input <- getLine
  case input of
    [c, n] -> do
      case fromCharToColor c of
        Just color -> do
          let cardPlayed = Card { color = color, number  = read [n] }
          let cardExist = cardPlayed `elem` userCards currentUser
          if cardExist && validCard (lastCardPlayed playedCards) cardPlayed
            then do
              let updatedUsers = updateUserCards users pos Play cardPlayed
              userPlay unusedCards (cardPlayed : playedCards) updatedUsers $ getPosition pos (length users)
            else tryAgain
        _    -> tryAgain
    "Q"      -> putStrLn "Exit game"
    "T"      -> do
      let takenCard = head unusedCards
      let updatedUsers = updateUserCards users pos Take takenCard
      userPlay (drop 1 unusedCards) playedCards updatedUsers pos
    _        -> tryAgain
  where
    tryAgain :: IO ()
    tryAgain = do
      putStrLn "Wrong card, please try again"
      userPlay unusedCards playedCards users pos

    updateUserCards :: Users -> Int -> Action -> Card -> Users
    updateUserCards users pos a card
      | a == Take = users & element pos .~ User { pos = pos, userCards = card : userCards (users !! pos) }
      | a == Play = users & element pos .~ User { pos = pos, userCards =  filter (/= card) $ userCards (users !! pos) }

    