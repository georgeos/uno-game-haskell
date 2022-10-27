module Game where

import Control.Lens.Operators ((.~), (&))
import Control.Lens (element)

import Types

startGame :: [Card] -> [Card] -> Users -> IO ()
startGame unusedCards playedCards users = do
  putStrLn ""
  putStrLn "--------------------------------------------------------"
  putStrLn "Start of game!"
  putStrLn "--------------------------------------------------------"
  userPlay unusedCards playedCards users 0

userPlay :: [Card] -> [Card] -> Users -> Int -> IO ()
userPlay unusedCards playedCards users pos = do
  putStrLn ""
  putStrLn $ "User " ++ show pos
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

          let playedCard = Card { color = color, number  = read [n] }
              cardExists = playedCard `elem` userCards currentUser
              validCard' = validCard (lastCardPlayed playedCards) playedCard

          if cardExists && validCard'
            then do

              let updatedUsers = updateUserCards users pos Play playedCard
                  playedCards' = playedCard : playedCards
                  newPosition  = getPosition pos (length users)

              if null (userCards $ updatedUsers !! pos)
                then putStrLn "¡¡¡YOU WIN!!!"
                else userPlay unusedCards playedCards' updatedUsers newPosition

            else tryAgain
        _    -> tryAgain

    "Q"      -> putStrLn "Exit game"
    "T"      -> do
      let unusedCards' = if null unusedCards then reverse $ tail playedCards else unusedCards
          playedCards' = if null unusedCards then [head playedCards] else playedCards
          takenCard    = head unusedCards'
          updatedUsers = updateUserCards users pos Take takenCard
      userPlay (drop 1 unusedCards') playedCards' updatedUsers pos
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

validCard :: Maybe Card -> Card -> Bool
validCard Nothing c2 = True
validCard (Just c1) c2 =
  color  c1 == color  c2 ||
  number c1 == number c2

getPosition :: Int -> Int -> Int
getPosition c t = if c < t - 1 then c + 1 else 0

lastCardPlayed :: [Card] -> Maybe Card
lastCardPlayed [] = Nothing
lastCardPlayed (c : cs) = Just c