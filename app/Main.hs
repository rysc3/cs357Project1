module Main where

-- imports 
import Score (getScoringData, getLetterScore)

main :: IO ()
main = do 
  -- Maybe we should define some global things here
  let dictionaryInputFile = "Dictionaries/01-Dictionary.txt"   -- dictionary
  let scoreInputFile = "Dictionaries/01-Scoring.txt"                   -- scoring     -- TODO figure out how to take user input for these
  let wordSize = 7                            -- # of letters we give the player, we can take input and set this to what they want to play with


  -- Testing reading input from dictionary
  -- start dictionary read test
  contents <- readFile dictionaryInputFile
  putStrLn $ "\n" ++ show (length (lines contents)) ++ " lines \n"
  -- print the first 10 lines
  mapM_ putStrLn $ take 10 $ lines contents
  -- end dictionary read test

  putStrLn "----------Score Test------------"
  scores <- getScoringData scoreInputFile

  putStrLn $ show scores

  putStrLn " -- A --"
  putStrLn $ show $ getLetterScore 'A' scores
  putStrLn " -- B --"
  putStrLn $ show $ getLetterScore 'B' scores
  putStrLn " -- C --"
  putStrLn $ show $ getLetterScore 'C' scores


  putStrLn "----------End Score Test------------"

  
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