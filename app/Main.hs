module Main where

-- Internal imports
import Dictionary(Trie, buildDictionary, contains, shrinkTrie, countWords, printTrie)
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
import Brick.Widgets.Border as BR (border)
import GHC.Base (build)
import Control.Monad.IO.Class (liftIO)
import System.Exit (exitSuccess)
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Table as T
import qualified Brick.Widgets.Border as B
import Brick.Types (Widget)
-- import Brick.Main as BR (App(..), defaultMain, neverShowCursor)
-- import Brick.Types as BR (Widget, BrickEvent(..), EventM, get, put)
-- import Brick.Widgets.Core as BR (str, withAttr, (<+>), vBox, hBox)
-- import Brick.AttrMap as BR (AttrMap, attrMap, AttrName, attrName)


main = do
  -- print your starting letters to terminal
  -- putStrLn "----- Starting Letters -----"
  -- startingLetters <- generateStartingLetters
  -- putStrLn startingLetters
  -- putStrLn "----------------------------"

  initialState <- initialize
  BR.defaultMain app initialState

data State = State
  { dictionary :: Trie,
    scoring :: [(Char, Int)],
    playedWords :: [(String, Int)],
    playedLetters :: String,
    availLetters :: String,
    score :: (Int, Int)
  }

initialize :: IO State
initialize = do
  -- Read in files
  dictionaryContents <- readFile "Dictionaries/01-Dictionary.txt"
  dictionary <- buildDictionary "Dictionaries/01-Dictionary.txt"
  scores <- getScoringData "Dictionaries/01-Scoring.txt"
  let score = (0,0)   -- initial score
  let playedWords = [("", 0)]

  -- Initialize State
  playedLetters <- return ""
  availLetters <- generateLetters dictionary  -- @ryan generate

  -- use AI to shrink trie
  -- putStrLn "--- Shrinking Dictionary ---"
  let actualSize = length $ lines dictionaryContents
  -- putStrLn $ "Actual Size: " ++ show actualSize
  -- putStrLn "---"

  let startDictionary = countWords dictionary
  -- putStrLn $ "Start: " ++ show startDictionary
  -- putStrLn "---"

  let shrunken = shrinkTrie availLetters dictionary

  let endDictionary = countWords shrunken
  -- putStrLn $ "End: " ++ show endDictionary
  -- putStrLn "---"
  -- putStrLn "Dictionary Shrunk"

  {-
    First we call generateStartingLetters to get a random list of 7 characters. 
    
    We pass the output here into generateLetters which 
      - shrinks dictionary 
      - counts the number of words in the dictionary 
      - if re-generates starting letters if the number of words is less than 30
    
    Once it makes it out of this method we have a valid let of starting letters. However, we don't allow duplicates. 

    So after the above two are completed, we then pass it into remove-duplicates to remove any words that have the same letter more than once. 
    This method will first remove these duplicates, but will conclude by checking the number of words in the trie after they've all been removed.
    If the number here is < 30, we will re-generate the starting letters again.

    Both of these conditions must be met for the game to start
  -}
  -- TODO @here I'm kinda just fibbin around about the number of words to estimate how many words have duplicate letters
  let score = (0, round $ fromIntegral (countWords shrunken) / 4)  -- save score to pass into initial state with num of possible words

  -- Print all words in shrunken dictionary
  -- let allWords = getAllWords shrunken
  -- mapM_ putStrLn allWords

  -- putStrLn "----------------------------"

  return State {dictionary = dictionary, scoring = scores, playedLetters = playedLetters, availLetters = availLetters, score = score, playedWords = playedWords}

{-
      -- Generate starting letters --
-}
generateLetters :: Trie -> IO String 
generateLetters dict = do 
  -- putStrLn "----------------"
  randomLetters <- generateStartingLetters
  isValid <- isValid randomLetters
  if isValid
    then do 
      -- testPossibleWords (shrinkTrie randomLetters dict) randomLetters
      return randomLetters 
    else do
      -- putStrLn "letters: "
      -- putStrLn randomLetters
      -- putStrLn "word:"
      let shrunkenTrie = shrinkTrie randomLetters dict
          wordCount = countWords shrunkenTrie
      -- print wordCount
      -- putStrLn "----------------"
      generateLetters dict
  where 
    isValid :: String -> IO Bool 
    isValid letters = do
      let shrunkenTrie = shrinkTrie letters dict
          wordCount = countWords shrunkenTrie
      return (wordCount > 1000)   -- Turned this way up so we don't run into any bad examples during the presentation. Still goes fast

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

testPossibleWords :: Trie -> String -> IO ()
testPossibleWords t s = do
  putStrLn $ " >> TESTING PRUNED DICT: "
  putStrLn $ "      set : " ++ s
  putStrLn $ "      size: " ++ (show $ countWords t)
  printTrie t

{-
      -- Generate starting letters --
-}

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


drawScore :: Int -> State -> BR.Widget ()
drawScore userScore s = BR.withBorderStyle BS.unicodeBold . B.borderWithLabel (BR.str "Score") . BR.vBox $
    [ BR.str $ "Total Score: " ++ show userScore
    , drawWordsCount s
    ]


drawUI :: State -> BR.Widget ()
drawUI s =
    let label = BR.withAttr (BR.attrName "label") . BR.str
        -- redBackgroundAttr = BR.withAttr (BR.attrName "redBackground") . BR.str -- I can't figure out how to set a background color
        borderLabel = BR.withBorderStyle BS.unicodeBold . B.borderWithLabel (label "Anagrams") . BR.padAll 1 
        content = BR.vBox
            [ BR.str "Welcome to Anagrams!"
            , BR.str "" -- Spacer
            , BR.hBox [drawPlayedLetters (playedLetters s), BR.str "", drawScore (fst (score s)) s] -- Horizontal layout for middle section
            , BR.hBox [drawavailLetters (availLetters s)] -- Horizontal layout for bottom section
            ]
        borderedContent = borderLabel content
        playedWordsWidget = drawPlayedWords (playedWords s)  -- New widget to display played words
        finalWidget = BR.vBox [BR.hBox [borderedContent, playedWordsWidget]]
    in
        C.center finalWidget

drawPlayedWords :: [(String, Int)] -> BR.Widget ()
drawPlayedWords playedWords =
    let label = BR.withAttr (BR.attrName "label") . BR.str
        borderLabel = BR.withBorderStyle BS.unicodeBold . B.borderWithLabel (label "Played Words") . BR.padAll 1 
        content = BR.vBox $ map drawWordWithScore $ filter (\(_, score) -> score /= 0) playedWords
    in
        borderLabel content

-- Function to draw a word with its score
drawWordWithScore :: (String, Int) -> BR.Widget ()
drawWordWithScore (word, score) =
    let wordWidget = BR.str word
        scoreWidget = BR.str ("  ~~   " ++ show score)
    in
        wordWidget BR.<+> scoreWidget 

drawWordsCount :: State -> BR.Widget ()
drawWordsCount s = BR.str $ "Words: " ++ show ((length $ playedWords s) - 1) ++ " / " ++ show (snd $ score s)

endOfGameGUI :: State -> Widget ()
endOfGameGUI s =
    let wordList = map fst (playedWords s)
        percent = (fromIntegral (length wordList) / fromIntegral (snd (score s)) * 100) * 4   -- We are rounding the number in initialize, must even that out here
        totalScore = fst (score s)
        wordText = "Words used: " ++ unwords wordList
        percentText = "Percentage of words found: " ++ show (round percent) ++ "%"
        scoreText = "Total score: " ++ show totalScore
        content = BR.vBox [BR.str wordText, BR.str percentText, BR.str scoreText]
    in
        BR.withBorderStyle BS.unicodeBold $ B.borderWithLabel (BR.str "Game Over") content
{-
      -- Draw Methods --
-}


handleEvent :: BR.BrickEvent () () -> BR.EventM () State ()
handleEvent (BR.VtyEvent (V.EvKey V.KEnter _)) = do
  s <- BR.get
  let word = playedLetters s
  if (contains word (dictionary s) && (not (elem word (map fst $ playedWords s))))
    then do
      let word_score = (getWordScore word (scoring s)) + (fst $ score s) -- increment user score
      -- liftIO $ putStrLn $ word ++ " is in trie | " ++ " new score: " ++ show word_score
      -- Add the valid word to the playedWords list
      let newPlayedWords = (word, (getWordScore word (scoring s))) : playedWords s
      BR.put $ s {playedLetters = "", availLetters = (availLetters s) ++ (playedLetters s), score = (word_score, snd $ score s), playedWords = newPlayedWords}
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
  BR.put $ s {playedLetters = "", availLetters = (availLetters s) ++ (playedLetters s)}
  return ()
handleEvent (BR.VtyEvent (V.EvKey V.KEsc _)) = do
    s <- BR.get
    let scoreWidget = drawScore (fst (score s)) s
    let endGameWidget = endOfGameGUI s
    let finalWidget = BR.vBox [scoreWidget, endGameWidget]
    liftIO $ BR.simpleMain finalWidget
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