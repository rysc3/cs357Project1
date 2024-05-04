module Main where

-- Internal imports

-- External imports
import Control.Monad (when)
-- things for leaderboard --

import Dictionary(Trie, buildDictionary, contains, getListOfStrings)
import Data.Time.Clock
import Data.Time.Format
import Score (getScoringData, getWordScore)

import Brick.Main (App(..), defaultMain, neverShowCursor, resizeOrQuit)
import Brick.Types (Widget, BrickEvent(..), EventM, get, put)
import Brick.Widgets.Core (str, withAttr, (<+>), vBox)
import Brick.Widgets.Table (table, renderTable)
import Brick.Widgets.Border (border)
import Brick.AttrMap (AttrMap, attrMap, AttrName, attrName)
import Brick.Util (fg)

import qualified Graphics.Vty as V

import Data.Char (isAlpha)




main :: IO ()
main = do 
  initialize >>= defaultMain app initialize

data State = State 
  { dictionary :: Trie,
    scoring :: [(Char, Int)],
    playedLetters :: String,
    availLetters:: String
  }

initialize :: State
initialize = State dictionary scoring "" "ABCDEF"
  where 
    dictionary = buildDictionary (loadDictionary "Dictionaries/01-Dictionary.txt")
    scoring = getScoringData "Dictionaries/01-Scoring.txt"
    loadDictionary :: FilePath -> IO [String]
    loadDictionary dictionaryInputFile = (readFile dictionaryInputFile) 

--removes letter from list of available letters
removeLetter :: Char -> [Char] -> [Char] 
removeLetter c avail = filter (/= c) avail

getLastLetter :: [Char] -> Char
getLastLetter [] = ' '
getLastLetter (x:xs) = if null xs then x else getLastLetter xs

--takes letter to be played, adds it to end of list of played letters 
addLetter :: Char -> [Char] -> [Char]
addLetter c xs = xs ++ [c]

addLetters :: String -> [Char] -> [Char]
addLetters [] avail = avail
addLetters (c:cs) avail = addLetters cs (removeLetter c avail)

drawavailLetters :: [Char] -> Widget ()
drawavailLetters avail = str $ "Your Letters: " ++ avail

drawPlayedLetters :: [Char] -> Widget ()
drawPlayedLetters played = str $ "Current Play: " ++ played

drawScore :: Int -> Widget ()
drawScore score = str $ "Total Score: " ++ show score

drawUI :: State -> [Widget ()]
drawUI s = [drawavailLetters (availLetters s), 
            drawPlayedLetters (playedLetters s),
            drawScore (getWordScore (playedLetters s) (scoring s))]

handleEvent :: BrickEvent () () -> EventM () State ()
handleEvent (VtyEvent (V.EvKey V.KEnter _)) = do
  s <- get
  let word = playedLetters s
  if contains word (dictionary s)
    then do
      let score = getWordScore word (scoring s)
      put $ s {playedLetters = "", availLetters = addLetters word (availLetters s)}
      return ()
    else return ()
handleEvent (VtyEvent (V.EvKey (V.KChar c) _)) = do
  s <- get
  put $ s {playedLetters = addLetter c (playedLetters s), availLetters = removeLetter c (availLetters s)}
  return ()
handleEvent (VtyEvent (V.EvKey (V.KChar back) _)) = do
  s <- get
  let lastLetter = getLastLetter (playedLetters s)
  put $ s {playedLetters = filter (/= lastLetter) (playedLetters s), availLetters = (availLetters s) ++ [lastLetter]}
  return ()

app :: App State () ()
app = App
    { appDraw         = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent  = handleEvent
    , appStartEvent   = return ()
    }