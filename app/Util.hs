module Util where

import Control.Monad.State
import System.Console.ANSI
import System.Console.ANSI.Types
import Types

printMaybeCard :: Maybe Card -> IO ()
printMaybeCard card =
  case card of
    Nothing -> liftIO $ putStrLn "Nothing played yet"
    Just c  -> liftIO $ printCard c >> putStrLn ""

printCards :: [Card] -> IO ()
printCards (c: cs) = printCard c >> putStr "," >> printCards cs
printCards _ = putStr ""

printCard :: Card -> IO ()
printCard card = do
  liftIO $ setSGR [SetConsoleIntensity BoldIntensity]
  case color card of
    R -> setSGR [SetColor Foreground Dull Red]
    Y -> setSGR [SetColor Foreground Dull Yellow]
    B -> setSGR [SetColor Foreground Dull Blue]
    G -> setSGR [SetColor Foreground Dull Green]
  putStr $ show card
  liftIO $ setSGR [Reset]
