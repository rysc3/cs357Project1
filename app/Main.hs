module Main where

-- Internal imports
import Tests (runAllTests)
import Score (getWordScore, getScoringData)
import Dictionary (buildDictionary, contains, getListOfStrings)

-- External imports
import Control.Monad (when)
import qualified Data.Map as Map
import Options.Applicative ()
import System.Exit (exitSuccess)
import System.Random (mkStdGen, randomRs)

-- | Main function to start the game
main :: IO ()
main = do
  putStrLn "Welcome to the game"
  putStrLn "Select an option:"
  putStrLn "1 - Play"
  putStrLn "2 - Leaderboard"
  putStrLn "3 - Settings"
  putStrLn "4 - Back/Quit"


  loop

{-
      -- BEGIN INITIALIZE LOGIC --
-}
loop :: IO ()
loop = do
  input <- getLine
  case input of
    "1" -> startGame
    "2" -> showLeaderboard
    "3" -> showSettings
    "4" -> quitGame
    _   -> do
      putStrLn "Invalid option. Please select again."
      loop

{-
  type Score = (Char, Int), which is what we use in Scoring Data.

  method initializes the default dictionary and scoring data
-}
initialize :: IO ([String], [(Char, Int)])
initialize = do
  -- Set the dictionary and scoring variables
  let dictionaryInputFile = "Dictionaries/01-Dictionary.txt"
      scoreInputFile = "Dictionaries/01-Scoring.txt"

  -- Load the dictionary and scoring data
  dictionary <- loadDictionary dictionaryInputFile
  scoring <- getScoringData scoreInputFile

  -- Return dictionary and scoring data
  return (dictionary, scoring)
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
  putStrLn "Starting the game..."
  (dictionary, scoring) <- initialize
  putStrLn "How many letters would you like to play with?"
  numLetters <- getNumberInput

  randomLetters <- generateRandomLetters numLetters
  putStrLn $ "Randomly selected letters: " ++ show randomLetters
  gameLoop dictionary scoring randomLetters

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
showLeaderboard = putStrLn "Showing the leaderboard..."

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
gameLoop :: [String] -> [(Char, Int)] -> String -> IO ()
gameLoop dictionary scoring randomLetters = do
  putStrLn "Enter a word:"
  word <- getLine
  putStrLn $ word ++ " => " ++ show (getWordScore word scoring)
  putStrLn $ "My Letters: " ++ show randomLetters
  putStrLn "Enter another word or press Enter to finish:"
  nextWord <- getLine
  if null nextWord
    then return ()  -- Finish the game loop
    else gameLoop dictionary scoring randomLetters


getLetterScore :: Char -> [(Char, Int)] -> Int
getLetterScore c scoring = case lookup c scoring of
  Just score -> score
  Nothing    -> 0

{-
      -- END MAIN GAME LOOP --
-}
