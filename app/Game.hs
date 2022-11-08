module Game where

import Control.Lens (element)
import Control.Lens.Operators ((&), (.~))
import Control.Monad.State
  ( MonadIO (liftIO),
    MonadState (get, put),
    StateT,
    void,
  )
import Data.Maybe (isNothing)
import System.Console.ANSI
  ( Color (Red),
    ColorIntensity (Dull),
    ConsoleIntensity (BoldIntensity),
    ConsoleLayer (Foreground),
    SGR (Reset, SetColor, SetConsoleIntensity),
    clearScreen,
    setSGR,
  )
import Types
  ( Action (..),
    Card (..),
    GameState (currentPos, playedCards, unusedCards, users),
    User (User, pos, userCards),
    Users,
    fromCharToColor,
  )
import Util (printCards, printMaybeCard)

startGame :: StateT GameState IO ()
startGame = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "--------------------------------------------------------"
  liftIO $ putStrLn "Start of game!"
  liftIO $ putStrLn "--------------------------------------------------------"
  void userPlay

userPlay :: StateT GameState IO ()
userPlay = do
  state <- get
  liftIO $ putStrLn ""
  liftIO $ putStrLn $ "User " ++ show (currentPos state)
  liftIO $ putStr "Last played card: "
  liftIO . printMaybeCard . lastCardPlayed $ playedCards state

  let currentUser = users state !! currentPos state

  liftIO $ putStr "Your cards: "
  liftIO $ printCards (userCards currentUser) >> putStrLn ""
  liftIO $ putStrLn "Enter your card to play or T to take one card or Q to quit: "
  input <- liftIO getLine

  case input of
    [c, n] -> do

      case fromCharToColor c of
        Just color -> do

          let playedCard = Card { color = color, number  = read [n] }
              cardExists = playedCard `elem` userCards currentUser
              validCard' = validCard (lastCardPlayed $ playedCards state) playedCard

          if cardExists && validCard'
            then do

              let updatedUsers = updateUserCards (users state) (currentPos state) Play playedCard
                  playedCards' = playedCard : playedCards state
                  newPosition  = getPosition (currentPos state) (length $ users state)

              if null (userCards $ updatedUsers !! currentPos state)
                then liftIO $ putStrLn "--------------------------------              ¡¡¡YOU WIN!!!            -----------------------------------"
                else do
                  liftIO clearScreen
                  put $ state { playedCards = playedCards', users = updatedUsers, currentPos  = newPosition }
                  void userPlay
            else tryAgain
        _    -> tryAgain

    "Q"      -> liftIO $ putStrLn "Exit game"
    "T"      -> do
      liftIO clearScreen
      let unusedCards' = if null $ unusedCards state then reverse $ tail (playedCards state) else unusedCards state
          playedCards' = if null $ unusedCards state then [head $ playedCards state] else playedCards state
          takenCard    = head unusedCards'
          updatedUsers = updateUserCards (users state) (currentPos state) Take takenCard
      put $ state { unusedCards = drop 1 unusedCards', playedCards = playedCards', users = updatedUsers }
      void userPlay
    _        -> tryAgain

  where
    tryAgain :: StateT GameState IO ()
    tryAgain = do
      liftIO clearScreen
      liftIO $ setSGR [SetConsoleIntensity BoldIntensity]
      liftIO $ setSGR [SetColor Foreground Dull Red]
      liftIO $ putStrLn "Wrong card, please try again"
      liftIO $ setSGR [Reset]
      void userPlay

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