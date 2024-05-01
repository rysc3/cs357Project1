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

data AppState = AppState
    { dictionary :: [String]
    , scoring :: [(Char, Int)]
    , leaderboardFile :: FilePath
    , gameState :: GameState
    }

data GameState = PreGame | Playing | ShowingLeaderboard | ShowingSettings

initialState :: IO AppState
initialState = do
  let dictionaryInputFile = "Dictionaries/01-Dictionary.txt"
      scoreInputFile = "Dictionaries/01-Scoring.txt"
      leaderboardFile = "Dictionaries/Leaderboard.csv"
  dictionary <- loadDictionary dictionaryInputFile
  scoring <- getScoringData scoreInputFile
  return $ AppState dictionary scoring leaderboardFile PreGame
  where
    loadDictionary :: FilePath -> IO [String]
    loadDictionary dictionaryInputFile = do
      contents <- readFile dictionaryInputFile
      return (lines contents)

app :: App AppState e ()
app = App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const $ attrMap V.defAttr []
    }

drawUI :: AppState -> [Widget ()]
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

drawContent :: AppState -> Widget ()
drawContent st =
    vBox [ str "How many letters would you like to play with?"
         , str ""
         , str "Press Enter to confirm"
         , str ""
         , str $ "Randomly selected letters: " ++ randomLetters
         ]
    where
        randomLetters = take (numLetters st) $ randomRs ('A', 'Z') (mkStdGen 42)

handleEvent :: AppState -> BrickEvent () e -> EventM () (Next AppState)
handleEvent st (VtyEvent (V.EvKey (V.KChar '1') [])) = continue $ st { gameState = Playing }
handleEvent st (VtyEvent (V.EvKey (V.KChar '2') [])) = continue $ st { gameState = ShowingLeaderboard }
handleEvent st (VtyEvent (V.EvKey (V.KChar '3') [])) = continue $ st { gameState = ShowingSettings }
handleEvent st (VtyEvent (V.EvKey (V.KChar '4') [])) = liftIO $ quitGame
handleEvent st (VtyEvent (V.EvKey V.KEnter [])) =
    case gameState st of
        Playing -> do
            let totalScore = calculateTotalScore st
            liftIO $ saveQuitGame totalScore
            continue $ st { totalScore = totalScore }
        _ -> continue st
handleEvent st _ = continue st

calculateTotalScore :: AppState -> Int
calculateTotalScore st = sum $ map snd (wordScores st)

parseLeaderboard :: AppState -> [(Int, String, String)]
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
sortLeaderboard = sortBy (flip compare `on` (\(score, _, _) -> score))

quitGame :: IO ()
quitGame = do
  putStrLn "Quitting the game..."
  exitSuccess

saveQuitGame :: Int -> IO ()
saveQuitGame totalScore = do
  putStrLn "------------------"
  putStrLn "Quitting the game..."
  putStrLn "Please enter your name:"
  putStrLn "---"
  name <- getLine
  putStrLn "---"
  currentTime <- getCurrentTime
  let formattedDate = formatTime defaultTimeLocale
