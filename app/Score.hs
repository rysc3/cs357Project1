-- -- () says we are essentially only making getScores public, the rest of the methods will not be accessible outside of the module
-- module Score (getScores) where 

--   -- TODO
--   -- We will figure out how to change these later when we take user input
--   let dictionary = "Dictionaries/01-Dictionary.txt"
--   let scoring    = "Dictionaries/01-Socring.txt"

--   dictionaryContent <- readFile dictionary    -- read in dictionary 
--   scoreContent <- readFile scoring            -- read in scoring

--   {-
--     Dictionaries/01-Scoring.txt should have the following format:
--     A 1 
--     B 2 
--     C 3 
--     <Letter> <Score>
    
--     Read all the input in, and store into a list of tuples [(Char, Int)]
--     String should be the filepath of the input
--   -}
--   -- getScores :: String -> IO [(Char, Int)]
--   -- getScores 


module Score (
  getScores  -- Exported function
) where 

import System.IO

dictionary :: FilePath
dictionary = "Dictionaries/01-Dictionary.txt"
-- dictionaryContent = readFile dictionary

scoring :: FilePath
scoring = "Dictionaries/01-Scoring.txt"
-- scoreContent = readFile scoring

getScores :: String -> String -> IO [(Char, Int)]
getScores = undefined
-- getScores dictionary scoring = do
--   scoreContent <- readFile scoring

--   let scores = map (\x -> (head x, read (tail x) :: Int)) $ map words $ lines scoreContent

  -- return scores

