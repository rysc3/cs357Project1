module Main where

-- Internal imports
import Tests (runAllTests)
import Score (getWordScore, getScoringData)
import Dictionary (buildDictionary, contains, getListOfStrings)

-- External imports
import Control.Monad (when)
import Options.Applicative ()
import System.Exit (exitSuccess)
import System.Random (mkStdGen, randomRs)

-- | Main function to start the game
main :: IO ()
main = do
  preGameLoop

{-
      -- BEGIN INITIALIZE LOGIC --
-}
preGameLoop :: IO ()
preGameLoop = do
  putStrLn "\n------------------"
  putStrLn "Select an option:"
  putStrLn "1 - Play"
  putStrLn "2 - Leaderboard"
  putStrLn "3 - Settings"
  putStrLn "4 - Back/Quit"
  putStrLn "\n---"
  input <- getLine
  putStrLn "---\n"
  case input of
    "1" -> startGame
    "2" -> showLeaderboard
    "3" -> showSettings
    "4" -> quitGame
    _   -> do
      putStrLn "Invalid option. Please select again."
      preGameLoop


initialize :: IO ([String], [(Char, Int)], FilePath)
initialize = do
  let dictionaryInputFile = "Dictionaries/01-Dictionary.txt"
      scoreInputFile = "Dictionaries/01-Scoring.txt"
      leaderboardFile = "Dictionaries/Leaderboard.csv"
  dictionary <- loadDictionary dictionaryInputFile
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
  putStrLn "\n------------------"
  putStrLn "Starting the game..."
  (dictionary, scoring, _) <- initialize
  putStrLn "How many letters would you like to play with?"
  putStrLn "\n---"
  numLetters <- getNumberInput
  putStrLn "---\n"
  randomLetters <- generateRandomLetters numLetters
  putStrLn $ "Randomly selected letters: " ++ show randomLetters
  gameLoop dictionary scoring randomLetters []

getNumberInput :: IO Int
getNumberInput = do
  input <- getLine
  let numLetters = read input :: Int
  if numLetters < 1 || numLetters > 7
    then do
      putStrLn "Invalid number of letters. Please enter a number between 1 and 7."
      getNumberInput  -- Retry input
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
  let leaderboardEntries = take 10 (lines contents)
  mapM_ putStrLn leaderboardEntries
  preGameLoop 

showSettings :: IO ()
showSettings = putStrLn "Showing the settings..."

quitGame :: IO ()
quitGame = do
  putStrLn "Quitting the game..."
  exitSuccess

{-
      -- END GAME STATES --
-}



{-
      -- START MAIN GAME LOOP --
-}
gameLoop :: [String] -> [(Char, Int)] -> String -> [(String, Int)] -> IO ()
gameLoop dictionary scoring randomLetters wordScores = do
  word <- getLine
  let score = getWordScore word scoring
      totalScore = sum $ map snd wordScores
      letterFormat = unwords $ map (\c -> [c] ++ replicate 5 ' ') randomLetters
  mapM_ (\(w, s) -> putStrLn $ w ++ replicate (10 - length w) '.' ++ replicate (6 - length (show s)) ' ' ++ show s) (reverse wordScores)
  putStrLn $ replicate 23 '-'
  putStrLn $ letterFormat ++ " => " ++ show totalScore
  if null word
    then return ()  -- Finish the game loop
    else gameLoop dictionary scoring randomLetters ((word, score) : wordScores)
