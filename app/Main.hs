module Main where

-- imports

import Control.Monad (when)
import qualified Data.Map as Map
import Dictionary (buildDictionary, contains, getListOfStrings)
import Options.Applicative
import Score (getLetterScore, getScoringData)
import System.Exit (exitSuccess)
import System.Random (mkStdGen, randomRs)
import Tests (runAllTests)

-- Define your data types here if necessary

-- | Main function to start the game
main :: IO ()
main = do
  -- Initialize the game
  (dictionary, scoring) <- initialize

  -- Prompt the user to pick the number of letters
  putStrLn "How many letters would you like to play with?"
  numLetters <- getNumberInput

  -- Generate random letters
  let randomLetters = take numLetters (randomRs ('A', 'Z') (mkStdGen 42))

  -- Print the selected letters
  putStrLn $ "Randomly selected letters: " ++ show randomLetters
  putStrLn "Game ready."

{-
  Allow user to give a number of items they'll want to use
-}
initialize :: IO ([String], Map.Map Char Int)
initialize = do
  -- Set the dictionary and scoring variables
  let dictionaryInputFile = "Dictionaries/01-Dictionary.txt"
      scoreInputFile = "Dictionaries/01-Scoring.txt"

  -- Load the dictionary and scoring data
  dictionary <- loadDictionary dictionaryInputFile
  scoring <- loadScoringData scoreInputFile

  -- Return dictionary and scoring data
  return (dictionary, scoring)

-- | Load the dictionary from file
loadDictionary :: FilePath -> IO [String]
loadDictionary dictionaryInputFile = do
  contents <- readFile dictionaryInputFile
  return (lines contents)

-- | Load the scoring data from file
loadScoringData :: FilePath -> IO (Map.Map Char Int)
loadScoringData scoreInputFile = do
  -- Implement your logic to load scoring data here
  -- For now, let's assume we have some default scoring data
  return (Map.fromList [('A', 1), ('B', 3), ('C', 3), ('D', 2), ('E', 1)])

-- | Get number input from the user
getNumberInput :: IO Int
getNumberInput = do
  input <- getLine
  case reads input of
    [(n, "")] -> return n
    _ -> do
      putStrLn "Invalid input. Please enter a valid number."
      getNumberInput
