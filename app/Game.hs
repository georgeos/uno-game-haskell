module Game where

import Control.Lens.Operators ((.~), (&))
import Control.Lens (element)

import Types

validCard :: Maybe Card -> Card -> Bool
validCard Nothing c2 = True
validCard (Just c1) c2 =
  color  c1 == color  c2 ||
  number c1 == number c2

startGame :: [Card] -> [Card] -> Users -> IO ()
startGame unusedCards playedCards users = do
  putStrLn ""
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
  putStrLn $ "Last played card: " ++ show (lastCardPlayed playedCards)
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
              if null (userCards $ updatedUsers !! pos)
                then putStrLn "¡¡¡YOU WIN!!!"
                else userPlay unusedCards (cardPlayed : playedCards) updatedUsers $ getPosition pos (length users)
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
