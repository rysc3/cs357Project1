# Project 1 - Anograms
## Ryan Scherbarth, Maisy Dunlavy, Molly Palko

## Overview
Upon starting the game, you'll be greeted with a few options:

## Multi-threading error
if you start and stop the game too many times in a row, you may run into the error: `Anagrams: user error (RTS doesn't support multiple OS threads (use ghc -threaded when linking))
`

If this is the case, go through the following to clean the cabal setup

      cabal clean 
      cabal build
      cabal run


### Playing the game

You guess words, once you can't think of any more, you hit escape. It will then show your stats. To play a letter just type the letter key on your keyboard. 

Conveniently, at any point, if you hit the backspace key, it will move all of your letters back into your inventory. 

The game will solve for all possible words and tell you what percentage of words you used at the end. The game uses both the Scrabble dictionary, as well as scoring scheme.


## Install
### Dependencies 
- Cabal 3.0
- GHCI 9.4.8
- brick >= 0.50.0

### Download & run
`git clone https://github.com/rysc3/proj1`

`cd proj1`

`cabal run`