# UNO Game

## Description
Basic version of UNO Game ( https://en.wikipedia.org/wiki/Uno_(card_game) ) developed in Haskell. This version only includes coloured cards to make it easy to play, as when all cards are included, game could take up to two hours 😄

## Game instructions:
- There are different cards composed by Color and Number
    - Color: Red (R), Blue (B), Green (G) and Yellow (Y)
    - Number: 0 to X, where X depends of the difficulty of the game
- Example: R1, B4, Y9, etc.
- Every player has a set of cards
- Every player must play a similar card (color or number) to the previous played card
- If the player doesn't have a similar card, then must take one card from the main set
- First player without having cards, WINS!

## Build

`cabal repl`

## Run

`cabal run`
