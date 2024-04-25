module Tests (
  runAllTests
) where

import Score (getScoringData, getLetterScore)
import Dictionary (getListOfStrings)

-- Function to run all tests
runAllTests :: FilePath -> FilePath -> IO ()
runAllTests dictionaryInputFile scoreInputFile = do
  putStrLn "---------- Running All Tests ------------"

  putStrLn "-- Dictionary --"
  testReadDictionary dictionaryInputFile

  putStrLn "-- Score --"
  testReadScoringData scoreInputFile
  testGetLetterScore scoreInputFile 'A'
  testGetLetterScore scoreInputFile 'B'
  testGetLetterScore scoreInputFile 'C'

  -- testGetListOfStrings dictionaryInputFile
  putStrLn "------------------------------------------"

-- Test: Check if dictionary file length is correct
testReadDictionary :: FilePath -> IO ()
testReadDictionary dictionaryInputFile = do
  contents <- readFile dictionaryInputFile
  let numLines = length $ lines contents
  putStrLn $ "Check dictionary file length: " ++ if numLines == 267753 then "Pass" else "Fail, expected 267753 but got " ++ show numLines

-- Test: Check if the first 50 lines of the dictionary file are extracted correctly
testGetListOfStrings :: FilePath -> IO ()
testGetListOfStrings dictionaryInputFile = do
  contents <- readFile dictionaryInputFile
  let first50Lines = take 50 $ lines contents
  putStrLn "Check first 50 lines of dictionary file:"
  mapM_ putStrLn first50Lines

-- Test: Check if scoring data is read correctly
testReadScoringData :: FilePath -> IO ()
testReadScoringData scoreInputFile = do
  scores <- getScoringData scoreInputFile
  let expectedScores = [('A',1),('B',2),('C',3),('D',4),('E',5),('F',6),('G',7),('H',8),('I',9),('J',10),('K',11),('L',12),('M',13),('N',14),('O',15),('P',16),('Q',17),('R',18),('S',19),('T',20),('U',21),('V',22),('W',23),('X',24),('Y',25),('Z',10000)]
  putStrLn $ "Check scoring data: " ++ if scores == expectedScores then "Pass" else "Fail, expected " ++ show expectedScores ++ " but got " ++ show scores

-- Test: Check letter score calculation
testGetLetterScore :: FilePath -> Char -> IO ()
testGetLetterScore scoreInputFile letter = do
  scores <- getScoringData scoreInputFile
  let expectedScore = case letter of
                       'A' -> 1
                       'B' -> 2
                       'C' -> 3
                       _   -> 0
      actualScore = getLetterScore letter scores
  putStrLn $ "Check letter score for " ++ [letter] ++ ": " ++ if actualScore == expectedScore then "Pass" else "Fail, expected " ++ show expectedScore ++ " but got " ++ show actualScore
