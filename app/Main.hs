module Main where

-- Internal imports
import Dictionary(Trie, buildDictionary, contains)
import Score (getScoringData, getWordScore)


-- External imports
import Control.Monad (when)
import Data.Time.Clock
import Data.Time.Format
import Data.Char (isAlpha)

import System.Random
import Data.Char (chr)
import Foreign.Marshal.Unsafe


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

main = do
  initialState <- initialize
  BR.defaultMain app initialState

data State = State
  { dictionary :: Trie,
    scoring :: [(Char, Int)],
    playedLetters :: String,
    availLetters :: String
  }

{-
  - Dictionary seems to be working
  - Scoring seems to be working 

  - still need to figure out tracking played letters 
  - Still need to generate random letters to start **
  -- Need AI for that part
-}
initialize :: IO State
initialize = do
  dictionary <- buildDictionary "Dictionaries/01-Dictionary.txt"
  scores <- getScoringData "Dictionaries/01-Scoring.txt"
  -- Test some example words
  let exampleWords = ["hello", "world", "example"]
  mapM_ (\word -> testGetWordScore scores word) exampleWords
  let playedLetters = "" -- TODO: figure this out
      availLetters = unsafeLocalState (getLetterSet 5 "ae") -- TODO: generate random letters on start
  return State {dictionary = dictionary, scoring = scores, playedLetters = playedLetters, availLetters = availLetters}

testGetWordScore :: [(Char, Int)] -> String -> IO ()
testGetWordScore scores word = do
  putStrLn $ " -- " ++ word ++ " --"
  putStrLn $ show $ getWordScore word scores

-- generate list of random letters
randomChar :: IO Char
randomChar = fmap chr (randomRIO (97, 122))

getSet :: Int -> IO [Char]
getSet n = sequence $ replicate n randomChar

getLetterSet :: Int -> [Char] -> IO [Char]
getLetterSet n ls = fmap (++ ls) (getSet n)
-- for 7 random letters use :  getLetterSet 7 []
-- for set always w "ae" use:  getLetterSet 5 "ae"

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

drawavailLetters :: [Char] -> BR.Widget ()
drawavailLetters avail = BR.str $ "Your Letters: " ++ avail

drawPlayedLetters :: [Char] -> BR.Widget ()
drawPlayedLetters played = BR.str $ "Current Play: " ++ played

drawScore :: Int -> BR.Widget ()
drawScore score = BR.str $ " Total Score: " ++ show score

drawUI :: State -> BR.Widget ()
drawUI s =
    let label = BR.withAttr (BR.attrName "label") . BR.str
        -- redBackgroundAttr = BR.withAttr (BR.attrName "redBackground") . BR.str -- I can't figure out how to set a background color
        borderLabel = BR.withBorderStyle BS.unicodeBold . B.borderWithLabel (label "Word Game")
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


defaultColor :: V.Color
defaultColor = V.black


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
  BR.put $ s {playedLetters = addLetter c (playedLetters s), availLetters = removeLetter c (availLetters s)}
  return ()
handleEvent (BR.VtyEvent (V.EvKey (V.KChar back) _)) = do
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