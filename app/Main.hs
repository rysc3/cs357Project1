module Main where

-- imports 
import Score (getScoringData, getWordScore)
import Tests (runAllTests)
import Dictionary (buildDictionary, contains)

main :: IO ()
main = do 
  -- Maybe we should define some global things here
  let dictionaryInputFile = "Dictionaries/01-Dictionary.txt"   -- dictionary
      scoreInputFile = "Dictionaries/01-Scoring.txt"                   -- scoring     -- TODO figure out how to take user input for these
      wordSize = 7                            -- # of letters we give the player, we can take input and set this to what they want to play with


  contents <- readFile dictionaryInputFile
  let dictionary = buildDictionary contents


  scores <- getScoringData scoreInputFile


  let word1 = "HELLO"
      word2 = "world"
      word3 = "apple"
      word4 = "banana"
      word5 = "orange"

  -- Test we can find letters
  putStrLn $ word1 ++ " exists? " ++ if contains word1 dictionary then "Yes" else "No"
  putStrLn $ word2 ++ " exists? " ++ if contains word2 dictionary then "Yes" else "No"
  putStrLn $ word3 ++ " exists? " ++ if contains word3 dictionary then "Yes" else "No"
  putStrLn $ word4 ++ " exists? " ++ if contains word4 dictionary then "Yes" else "No"
  putStrLn $ word5 ++ " exists? " ++ if contains word5 dictionary then "Yes" else "No"

  -- Test we have scores for letters
  testGetWordScores [word1, word2, word3, word4, word5] scores
  


  {-
    Function to check if a word exists in the dictionary.
      - Takes a String as input 
      - Takes a String for the dictionary name (since we might have multiple)
      - Returns the String if it exists
      - Returns null if it isn't in the dictionary
    
    We can probably change this to return a bool later, I think returning the word will make troubleshooting 
    easier at first. 
    
    findWord <SEARCH-TERM> -> <DICTIONARY-FILE-NAME> -> <RETURN>

    ** reference shrinkDictionary
  -}
  -- findWord :: [Char] -> [Char] -> [Char]
  -- findWord x y = undefined


  {-
    Function to shrink the dictionary, lets just take in the wordSize and return a subdictionary that only contains 
    1. words of <= wordSize
    2. words that only contain the user's letters

    Takes in an int and [Char], and retunrs a [[Char]] with all valid words in our new subdictionary
  -}
  -- shrinkDictionary :: int -> [Char] -> [[Char]]

  {-
    Calculate score function. 
      - Takes a string as input 
      - returns an int as output
    
    Here, we can just hard-code values for each letter, similar to how scrabble works, and add a multiplier for length of word

  -}
  -- scoreWord :: [Char] -> int
  -- scoreWord x = undefined

  {-
    Fucntion takes scoring and returns tuple (<LETTER>, <SCORE>)
  -}
  -- scores :: [Char] -> (Char, Int)
  -- -- read the scoring input
  -- contents = readFile scoring
  -- scores xs = (head xs, read (tail xs) :: 1)
  -- putStrLn $ show $ scores "A"

  
  -- Keep here so we run the tests in the github pipeline
  runAllTests dictionaryInputFile scoreInputFile

-- Takes a list of words and checks their scores to make sure dictionary & scoring is intiialized properly
testGetWordScores :: [String] -> [(Char, Int)] -> IO ()
testGetWordScores [] _ = return ()
testGetWordScores (x:xs) scores = do
  putStrLn $ "Score for " ++ x ++ " is " ++ show (getWordScore x scores)
  testGetWordScores xs scores
