module Main where

-- Internal imports

-- External imports
import Control.Monad (when)
-- things for leaderboard --

import Dictionary(Trie, buildDictionary, contains, getListOfStrings)
import Data.Function (on)
import Data.List (insertBy, sortBy)
import Data.Time.Clock
import Data.Time.Format
import Dictionary (buildDictionary, contains, getListOfStrings)
import Score (getScoringData, getWordScore)
import System.Exit (exitSuccess)
import System.IO
import System.Random (mkStdGen, randomRs)
import Tests (runAllTests)
import Data.Maybe (mapMaybe, catMaybes)
import Control.Monad (forM)


main :: IO ()
main = do
  preGameLoop

{-
      -- BEGIN INITIALIZE LOGIC --
-}
preGameLoop :: IO ()
preGameLoop = do
  putStrLn "------------------"
  putStrLn "Select an option:"
  putStrLn "1 - Play"
  putStrLn "2 - Leaderboard"
  putStrLn "3 - Settings"
  putStrLn "4 - Back/Quit"
  putStrLn "\n---"
  input <- getLine
  putStrLn "---"
  case input of
    "1" -> startGame
    "2" -> showLeaderboard
    "3" -> showSettings
    "4" -> quitGame
    _ -> do
      putStrLn "Invalid option. Please select again."
      preGameLoop

initialize :: IO (Trie, [(Char, Int)], FilePath)
initialize = do
  let dictionaryInputFile = "Dictionaries/01-Dictionary.txt"
      scoreInputFile = "Dictionaries/01-Scoring.txt"
      leaderboardFile = "Dictionaries/Leaderboard.csv"
      dictionaryInput = loadDictionary dictionaryInputFile
  dictionary <- buildDictionary <$> dictionaryInput
  scoring <- getScoringData scoreInputFile
  return (dictionary, scoring, leaderboardFile)
  where
    loadDictionary :: FilePath -> IO [String]
    loadDictionary dictionaryInputFile = do
      contents <- readFile dictionaryInputFile
      return (lines contents)

{-
      -- END INITIALIZE LOGIC --
-}



{-
      -- START GAME STATES --
-}
startGame :: IO ()
startGame = do
  putStrLn "------------------"
  putStrLn "Starting the game..."
  (dictionary, scoring, _) <- initialize
  putStrLn "How many letters would you like to play with?"
  putStrLn "\n---"
  numLetters <- getNumberInput
  putStrLn "---"
  randomLetters <- generateRandomLetters numLetters
  putStrLn $ "Randomly selected letters: " ++ show randomLetters
  -- TODO @ryan figure out the spacing here
  -- putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
  -- putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
  -- putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
  gameLoop dictionary scoring randomLetters 0 []

getNumberInput :: IO Int
getNumberInput = do
  input <- getLine
  let numLetters = read input :: Int
  if numLetters < 1 || numLetters > 7
    then do
      putStrLn "Invalid number of letters. Please enter a number between 1 and 7."
      getNumberInput -- Retry input
    else return numLetters

generateRandomLetters :: Int -> IO String
generateRandomLetters numLetters = do
  let randomLetters = take numLetters (randomRs ('A', 'Z') (mkStdGen 42))
  return randomLetters

showLeaderboard :: IO ()
showLeaderboard = do
  putStrLn "Showing the leaderboard..."
  (_, _, leaderboardFile) <- initialize
  contents <- readFile leaderboardFile
  parsedEntries <- forM (lines contents) parseEntry
  let numLines = length $ lines contents
  let validEntries = catMaybes parsedEntries
      sortedEntries = sortLeaderboard validEntries
      numEntries = length sortedEntries
      maxEntriesToShow = min 10 numEntries
      topEntries = take maxEntriesToShow sortedEntries
  mapM_ putStrLn (map formatEntry topEntries)
  preGameLoop

showSettings :: IO ()
showSettings = do
  putStrLn "At some point, I'll put some settings here"
  putStrLn "1 - Back"
  input <- getLine
  case input of
    "1" -> preGameLoop
    _ -> do
      putStrLn "Invalid option. Please select again."
      showSettings

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
  let formattedDate = formatTime defaultTimeLocale "%-m/%-d" currentTime
  putStrLn "------------------"
  putStrLn $ "Today's date: " ++ formattedDate
  addToLeaderboard totalScore name
  putStrLn "Saving to leaderboard"
  putStrLn "------------------"
  preGameLoop -- go back to title screen

{-
      -- END GAME STATES --
-}



{-
      -- START LEADERBOARD LOGIC --
-}
addToLeaderboard :: Int -> String -> IO ()
addToLeaderboard score name = do
  currentTime <- getCurrentTime
  let date = formatTime defaultTimeLocale "%m/%d" currentTime
      newEntry = (score, date, name)
  withFile "Dictionaries/Leaderboard.csv" AppendMode $ \handle -> do
    hPutStrLn handle (formatEntry newEntry)

sortLeaderboard :: [(Int, String, String)] -> [(Int, String, String)]
sortLeaderboard = sortBy (flip compare `on` (\(score, _, _) -> score))

{-
  Logic to parse leaderboard and sort. 

  Everything is saved in Dictionaries/Leaderboard.csv, but it's actually pretty difficult to parse that data. it's easy to parse the beginning portion but then
  When you get to the last value of the list it was having issues with the null case, when you try to read line n+1 of the file. 

  I had to make a helper function, wordsWhen, that splits and skips once we get to this point so we don't just hit a terminal error, then it just returns 
  back to parseEntry which sorts and prints everyting.
-}
parseEntry :: String -> IO (Maybe (Int, String, String))
parseEntry entry
  | null entry = return Nothing  -- Skip empty lines
  | otherwise = do
      let parts = wordsWhen (== ',') entry
      case parts of
        [scoreStr, date, name] -> return $ Just (read scoreStr, date, name)
        _ -> do
          return Nothing

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


-- debugParseEntry :: String -> IO (Int, String, String)
-- debugParseEntry entry = do
--     putStrLn $ "Parsing entry: " ++ entry
--     return $ parseEntry entry

formatEntry :: (Int, String, String) -> String
formatEntry (score, date, name) = show score ++ ", " ++ date ++ ", " ++ name

{-
      -- END LEADERBOARD LOGIC --
-}



{-
      -- START MAIN GAME LOOP --
-}
gameLoop :: Trie -> [(Char, Int)] -> String -> Int -> [(String, Int)] -> IO ()
gameLoop dictionary scoring randomLetters totalScore wordScores = do
  word <- getLine
  if (not . contains word) dictionary
    then do
      putStrLn "Invalid word. Please try again."
      gameLoop dictionary scoring randomLetters totalScore wordScores
  else
    let score = getWordScore word scoring
        newTotalScore = totalScore + score
        letterFormat = unwords $ map (\c -> [c] ++ replicate 5 ' ') randomLetters
    mapM_ (\(w, s) -> putStrLn $ w ++ replicate (10 - length w) '.' ++ replicate (6 - length (show s)) ' ' ++ show s) (reverse wordScores)
    putStrLn $ replicate 23 '-'
    putStrLn $ letterFormat ++ " => " ++ show newTotalScore
  if null word
    then saveQuitGame newTotalScore
    else gameLoop dictionary scoring randomLetters newTotalScore ((word, score) : wordScores)