module Main where

import Control.Monad (forM, when)
import Data.Function (on)
import Data.List (insertBy, sortBy)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Time.Clock
import Data.Time.Format
import System.Exit (exitSuccess)
import System.IO
import System.Random (mkStdGen, randomRs)

import Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (vCenter)
import qualified Graphics.Vty as V

main :: IO ()
main = preGameLoop

data State = State
    { dictionary :: [String]
    , scoring :: [(Char, Int)]
    , leaderboardFile :: FilePath
    , gameState :: GameState
    , totalScore :: Int
    }

data GameState = PreGame | Playing | ShowingLeaderboard | ShowingSettings

initialState :: IO State
initialState = do
  let dictionaryInputFile = "Dictionaries/01-Dictionary.txt"
      scoreInputFile = "Dictionaries/01-Scoring.txt"
      leaderboardFile = "Dictionaries/Leaderboard.csv"
  dictionary <- loadDictionary dictionaryInputFile
  scoring <- getScoringData scoreInputFile
  return $ State dictionary scoring leaderboardFile PreGame 0
  where
    loadDictionary :: FilePath -> IO [String]
    loadDictionary dictionaryInputFile = do
      contents <- readFile dictionaryInputFile
      return (lines contents)

app :: App State e ()
app = App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const $ attrMap V.defAttr []
    }

drawUI :: State -> [Widget ()]
drawUI st =
    case gameState st of
        PreGame -> drawPreGameUI
        Playing -> drawPlayingUI st
        ShowingLeaderboard -> drawLeaderboardUI st
        ShowingSettings -> drawSettingsUI
    where
        drawPreGameUI =
            [ border $ vCenter $ hCenter $
                vBox [ str "Select an option:"
                     , str "1 - Play"
                     , str "2 - Leaderboard"
                     , str "3 - Settings"
                     , str "4 - Quit"
                     ]
            ]

        drawPlayingUI st' =
            [ border $ vCenter $ hCenter $ drawContent st'
            ]

        drawLeaderboardUI st' =
            [ border $ vCenter $ hCenter $
                vBox $ map (str . formatEntry) $ take 10 $ sortLeaderboard $ parseLeaderboard st'
            ]

        drawSettingsUI =
            [ border $ vCenter $ hCenter $
                vBox [ str "At some point, I'll put some settings here"
                     , str "1 - Back"
                     ]
            ]

drawContent :: State -> Widget ()
drawContent st =
    vBox [ str "How many letters would you like to play with?"
         , str ""
         , str "Press Enter to confirm"
         , str ""
         , str $ "Randomly selected letters: " ++ randomLetters
         ]
    where
        randomLetters = take (numLetters st) $ randomRs ('A', 'Z') (mkStdGen 42)





calculateTotalScore :: State -> Int
calculateTotalScore st = sum $ map snd (wordScores st)

parseLeaderboard :: State -> [(Int, String, String)]
parseLeaderboard st =
    let contents = readFile (leaderboardFile st)
        parsedEntries = mapMaybe parseEntry contents
    in sortLeaderboard parsedEntries

parseEntry :: String -> Maybe (Int, String, String)
parseEntry entry
    | null entry = Nothing  -- Skip empty lines
    | otherwise =
        let parts = wordsWhen (== ',') entry
        in case parts of
            [scoreStr, date, name] -> Just (read scoreStr, date, name)
            _ -> Nothing

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

formatEntry :: (Int, String, String) -> String
formatEntry (score, date, name) = show score ++ ", " ++ date ++ ", " ++ name

sortLeaderboard :: [(Int, String, String)] -> [(Int, String, String)]
sortLeaderboard = sortBy (flip compare `Data.Function.on` (\(score, _, _) -> score))

quitGame :: IO ()
quitGame = do
  putStrLn "Quitting the game..."
  exitSuccess

saveQuitGame :: State -> Int -> IO ()
saveQuitGame st totalScore = do
  putStrLn "------------------"
  putStrLn "Quitting the game..."
  putStrLn "Please enter your name:"
  putStrLn "---"
  name <- getLine
  putStrLn "---"
  currentTime <- getCurrentTime
  let formattedDate = formatTime defaultTimeLocale "%-m/%-d" currentTime
  putStrLn "------------------"
  putStrLn $ "Today's date: " ++ formattedDate
  addToLeaderboard st totalScore name
  putStrLn "Saving to leaderboard"
  putStrLn "------------------"
  preGameLoop -- go back to title screen

addToLeaderboard :: State -> Int -> String -> IO ()
addToLeaderboard st score name = do
  currentTime <- getCurrentTime
  let date = formatTime defaultTimeLocale "%m/%d" currentTime
      newEntry = (score, date, name)
  withFile (leaderboardFile st) AppendMode $ \handle -> do
    hPutStrLn handle (formatEntry newEntry)

preGameLoop :: IO ()
preGameLoop = do
  putStrLn "------------------"
  putStrLn "Select an option:"
  putStrLn "1 - Play"
  putStrLn "2 - Leaderboard"
  putStrLn "3 - Settings"
  putStrLn "4 - Quit"
  putStrLn "\n---"
  initialState >>= defaultMain app

