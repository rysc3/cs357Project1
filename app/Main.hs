module Main where

-- Internal imports
import Dictionary(Trie, buildDictionary, contains, shrinkTrie, countWords, getAllWords)
import Score (getScoringData, getWordScore)


-- External imports
import Control.Monad (when)
import Data.Time.Clock
import Data.Time.Format
import Data.Char (isAlpha, toUpper)
import System.Random (randomRIO)
import Data.List (nub)

-- Brick things --
import qualified Graphics.Vty as V
import qualified Brick as BR
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border.Style as BS
-- import Brick.Main as BR (App(..), defaultMain, neverShowCursor)
-- import Brick.Types as BR (Widget, BrickEvent(..), EventM, get, put)
-- import Brick.Widgets.Core as BR (str, withAttr, (<+>), vBox, hBox)
import Brick.Widgets.Border as BR (border)
import GHC.Base (build)
import Control.Monad.IO.Class (liftIO)
import System.Exit (exitSuccess)
-- import Brick.AttrMap as BR (AttrMap, attrMap, AttrName, attrName)
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Table as T
import qualified Brick.Widgets.Border as B
import Brick.Types (Widget)

main = do
  -- print your starting letters to terminal
  putStrLn "----- Starting Letters -----"
  startingLetters <- generateStartingLetters
  putStrLn startingLetters
  putStrLn "----------------------------"

  initialState <- initialize
  BR.defaultMain app initialState

data State = State
  { dictionary :: Trie,
    scoring :: [(Char, Int)],
    playedLetters :: String,
    availLetters :: String
  }

initialize :: IO State
initialize = do
  -- Read in files
  dictionaryContents <- readFile "Dictionaries/01-Dictionary.txt"
  dictionary <- buildDictionary "Dictionaries/01-Dictionary.txt"
  scores <- getScoringData "Dictionaries/01-Scoring.txt"

  -- Initialize State
  playedLetters <- return ""
  availLetters <- generateLetters dictionary  -- @ryan generate

  -- use AI to shrink trie
  putStrLn "--- Shrinking Dictionary ---"
  let actualSize = length $ lines dictionaryContents
  putStrLn $ "Actual Size: " ++ show actualSize
  putStrLn "---"

  let startDictionary = countWords dictionary
  putStrLn $ "Start: " ++ show startDictionary
  putStrLn "---"

  let shrunken = shrinkTrie availLetters dictionary

  let endDictionary = countWords shrunken
  putStrLn $ "End: " ++ show endDictionary
  putStrLn "---"
  putStrLn "Dictionary Shrunk"

  -- Print all words in shrunken dictionary
  let allWords = getAllWords shrunken
  mapM_ putStrLn allWords

  putStrLn "----------------------------"

  return State {dictionary = dictionary, scoring = scores, playedLetters = playedLetters, availLetters = availLetters}


generateLetters :: Trie -> IO String
generateLetters dict = do 
  -- Generate the first set of letters 
  randomLetters <- generateStartingLetters

  isValid <- isValid randomLetters
  if isValid
    then return randomLetters 
    else generateLetters dict
  where 
    -- check for 30
    isValid :: String -> IO Bool 
    isValid letters = do
      let shrunkenTrie = shrinkTrie letters dict
          wordCount = countWords shrunkenTrie
      return (wordCount > 30)


generateStartingLetters :: IO String
generateStartingLetters = do
  -- Always start with A and E so there will be enough words that can be generated
  let startingLetters = ['A', 'E']
  -- get list of 7 characters
  finalChars <- getMoreChars startingLetters
  return finalChars
  where
    getMoreChars :: String -> IO String
    getMoreChars chars
      | length chars == 7 = return chars
      | otherwise = do
          -- Generate a random character
          randomChar <- generateRandomChar
          if randomChar `notElem` chars
            then getMoreChars (randomChar : chars)
            else getMoreChars chars

    generateRandomChar :: IO Char
    generateRandomChar = randomRIO ('B', 'Z')


removeLetter :: Char -> [Char] -> [Char]
removeLetter c avail = filter (/= c) avail

getLastLetter :: [Char] -> Char
getLastLetter [] = ' '
getLastLetter (x : xs) = if null xs then x else getLastLetter xs

addLetter :: Char -> [Char] -> [Char]
addLetter c xs = xs ++ [c]

addLetters :: String -> [Char] -> [Char]
addLetters [] avail = avail
addLetters (c : cs) avail = addLetters cs (removeLetter c avail)


{-
      -- Draw Methods --
-}

defaultColor :: V.Color
defaultColor = V.black

type TableCell = BR.Widget ()

-- Function to draw available letters in a table
drawavailLetters :: [Char] -> Widget ()
drawavailLetters avail =
    let paddedChars = take 7 (avail ++ repeat ' ') -- Ensure we have at least 7 characters, padding with spaces if necessary
        cells = map (\c -> B.border (BR.padLeftRight 1 $ BR.str [c])) paddedChars
        table = BR.hBox cells
    in
        B.borderWithLabel (BR.str "Available Letters") table

-- Function to draw played letters in a table
drawPlayedLetters :: [Char] -> Widget ()
drawPlayedLetters played =
    let paddedChars = take 7 (played ++ repeat ' ') -- Ensure we have at least 7 characters, padding with spaces if necessary
        cells = map (\c -> B.border (BR.padLeftRight 1 $ BR.str [c])) paddedChars
        table = BR.hBox cells
    in
        B.borderWithLabel (BR.str "Played Letters") table


drawScore :: Int -> BR.Widget ()
drawScore score = BR.str $ "Total Score: " ++ show score

drawUI :: State -> BR.Widget ()
drawUI s =
    let label = BR.withAttr (BR.attrName "label") . BR.str
        -- redBackgroundAttr = BR.withAttr (BR.attrName "redBackground") . BR.str -- I can't figure out how to set a background color
        borderLabel = BR.withBorderStyle BS.unicodeBold . B.borderWithLabel (label "Word Game") . BR.padAll 1 
        content = BR.vBox
            [ BR.str "Welcome to Word Game!"
            , BR.str "" -- Spacer
            , BR.hBox [drawPlayedLetters (playedLetters s), BR.str ""] -- Horizontal layout for middle section
            , BR.hBox [drawavailLetters (availLetters s), drawScore (getWordScore (playedLetters s) (scoring s))] -- Horizontal layout for bottom section
            ]
        borderedContent = borderLabel content
        -- Widget with yellow background and borders all around
        finalWidget = BR.withAttr (BR.attrName "redBackground") borderedContent
    in
        C.center finalWidget


{-
      -- Draw Methods --
-}


handleEvent :: BR.BrickEvent () () -> BR.EventM () State ()
handleEvent (BR.VtyEvent (V.EvKey V.KEnter _)) = do
  s <- BR.get
  let word = playedLetters s
  if contains word (dictionary s)
    then do
      let score = getWordScore word (scoring s)
      liftIO $ putStrLn $ word ++ " is in trie | " ++ " score: " ++ show score
      BR.put $ s {playedLetters = "", availLetters = addLetters word (availLetters s)}
      return ()
    else do
      BR.put $ s {playedLetters = "", availLetters = (availLetters s) ++ (playedLetters s)}
      return ()
handleEvent (BR.VtyEvent (V.EvKey (V.KChar c) _)) = do
  s <- BR.get
  if elem (toUpper c) (availLetters s)
    then do
      BR.put $ s {playedLetters = addLetter (toUpper c) (playedLetters s), availLetters = removeLetter (toUpper c) (availLetters s)}
      return ()
    else do
      return ()
handleEvent (BR.VtyEvent (V.EvKey V.KBS _)) = do
  s <- BR.get
  let lastLetter = getLastLetter (playedLetters s)
  BR.put $ s {playedLetters = filter (/= lastLetter) (playedLetters s), availLetters = (availLetters s) ++ [lastLetter]}
  return ()
handleEvent (BR.VtyEvent (V.EvKey V.KEsc _)) = do 
  liftIO $ putStrLn "Quitting Game"
  liftIO exitSuccess


app :: BR.App State () ()
app =
  BR.App
    { BR.appDraw = \s -> [drawUI s],
      BR.appChooseCursor = BR.neverShowCursor,
      BR.appHandleEvent = handleEvent,
      BR.appStartEvent = return (),
      BR.appAttrMap = const $ BR.attrMap V.defAttr [] -- I don't really know what this actually does but it makes things work lol
    }