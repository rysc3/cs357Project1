module Score (
  getScoringData,  -- Public methods main can see
  getLetterScore,
  getWordScore
) where 

import Data.Char (toUpper)  -- for getScores method

type Score = (Char, Int)

parseLine :: String -> Score
parseLine line =
  case words line of
    [char, score] -> (head char, read score :: Int)
    _             -> error "should be char, score"

readScoresFromFile :: FilePath -> IO [Score]
readScoresFromFile filePath = do
  content <- readFile filePath
  return $ map parseLine (lines content)




--------------- Public Methods ----------------


{-
  Method loops through the input and returns a list of tuples, of type (<Char>, <Int>), where the int is the corresponding score 
  for that letter. 

  Input files should be in the format:

  A 1
  B 2
  C 3
  ...
-}
getScoringData :: FilePath -> IO [Score]
getScoringData filePath = do 
  scores <- readScoresFromFile filePath
  return $ map (\(char, score) -> (toUpper char, score)) scores   -- convert all letters to uppercase

{-
  Breaks down the Score type, we pass in a character and our score type, and it will return the integer score for that character. 

  Break down input into two pieces, current and rest.
  return 0 for any invalid inputs.
  if input x matches the current Char, return the int in that tuple
  otherwise, recurse on the remaining list.
-}
getLetterScore :: Char -> [Score] -> Int
getLetterScore _ [] = 0
getLetterScore x ((char, score):rest) 
  | x == char = score
  | otherwise = getLetterScore x rest

{-
  Calculates the score of a given word by calculating the score of each letter, and also adding on a letter count multiplier

  There is no multiplier for < 3 letter words. 
  on the range [3,7] we have a multiplier of 7x where the x is the number of leters in the word
  So if you have a word 
  `aaa` = 3 pts 
  `aaaa` = 3 * 1 + 1*7 = 10 pts   -- 4th letter multiplied
  `aaaaa` = 3 * 1 + 2*7 = 17 pts  -- 4th and 5th letter multiplied
  ...
-}
getWordScore :: [Char] -> [Score] -> Int
getWordScore [] _ = 0
getWordScore (x:xs) scores = getLetterScore x scores + getWordScore xs scores