# Project 1 - Anograms
## Ryan Scherbarth, Maisy Dunlavy, Molly Palko

## Overview
Upon starting the game, you'll be greeted with a few options:

### Playing the game

      ------------------
      Select an option:
      1 - Play
      2 - Leaderboard
      3 - Settings
      4 - Back/Quit

      ---

If you select Play, you'll be prompted to choose the number of letters you'd like to use, and then the game will begin:

      Starting the game...
      How many letters would you like to play with?

      ---
      5
      ---
      Randomly selected letters: "TBPJL"
      a   
      -----------------------
      T      B      P      J      L      => 1
      b
      a.........     1
      -----------------------
      T      B      P      J      L      => 3
      c
      a.........     1
      b.........     2
      -----------------------
      T      B      P      J      L      => 6
      d
      a.........     1
      b.........     2
      c.........     3
      -----------------------
      T      B      P      J      L      => 10

Once you've completed the game, you'll be prompted for a name, after which your score, name, and today's date will be recorded onto the leaderboard. You'll then be brought back to the main screen.


### Leaderboard 
There is also a leaderboard, stored internally at `Dictionaries/Leaderbaord.csv`. You can access it by entering `2` from the start menu.
This will show you the top 10 scores, in sorted order, with the corresponding dates and usernames 

      ------------------
      Select an option:
      1 - Play
      2 - Leaderboard
      3 - Settings
      4 - Back/Quit

      ---
      2
      ---
      Showing the leaderboard...
      1590000,  05/01,  best
      211633,  05/01,  ryan
      123099,  9/1,  bobby
      10553,  05/01,  imthebest
      10000,  4/10,  Ryan 
      9999,   4/01,  Ryan
      8274,  5/24,  myScore
      1221,  5/5,  dan
      494,  05/01,  another
      468,  05/01,  asdf





## Install
### Dependencies 
- Cabal 3.0
- GHCI 9.4.8
- brick >= 0.50.0

### Usage 
`git clone https://github.com/rysc3/proj1`
`cd proj1`
`cabal run`