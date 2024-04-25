module Tests
  ( runAllTests,
  )
where

import Dictionary (getListOfStrings)
import Score (getLetterScore, getScoringData, getWordScore)

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
  testGetWordScoreABC scoreInputFile
  testGetWordScoreABAM scoreInputFile
  testGetWordScoreABNOP scoreInputFile
  putStrLn "------------------------------------------"

testReadDictionary :: FilePath -> IO ()
testReadDictionary dictionaryInputFile = do
  contents <- readFile dictionaryInputFile
  let numLines = length $ lines contents
  putStrLn $ "Check dictionary file length: " ++ if numLines == 267753 then "Pass" else "Fail, expected 267753 but got " ++ show numLines

testGetListOfStrings :: FilePath -> IO ()
testGetListOfStrings dictionaryInputFile = do
  contents <- readFile dictionaryInputFile
  let first50Lines = take 50 $ lines contents
  putStrLn "Check first 50 lines of dictionary file:"
  mapM_ putStrLn first50Lines

testReadScoringData :: FilePath -> IO ()
testReadScoringData scoreInputFile = do
  scores <- getScoringData scoreInputFile
  let expectedScores = [('A', 1), ('B', 2), ('C', 3), ('D', 4), ('E', 5), ('F', 6), ('G', 7), ('H', 8), ('I', 9), ('J', 10), ('K', 11), ('L', 12), ('M', 13), ('N', 14), ('O', 15), ('P', 16), ('Q', 17), ('R', 18), ('S', 19), ('T', 20), ('U', 21), ('V', 22), ('W', 23), ('X', 24), ('Y', 25), ('Z', 10000)]
  putStrLn $ "Check scoring data: " ++ if scores == expectedScores then "Pass" else "Fail, expected " ++ show expectedScores ++ " but got " ++ show scores

testGetLetterScore :: FilePath -> Char -> IO ()
testGetLetterScore scoreInputFile letter = do
  scores <- getScoringData scoreInputFile
  let expectedScore = case letter of
        'A' -> 1
        'B' -> 2
        'C' -> 3
        _ -> 0
      actualScore = getLetterScore letter scores
  putStrLn $ "Check letter score for " ++ [letter] ++ ": " ++ if actualScore == expectedScore then "Pass" else "Fail, expected " ++ show expectedScore ++ " but got " ++ show actualScore

testGetWordScoreABC :: FilePath -> IO ()
testGetWordScoreABC scoreInputFile = do
  scores <- getScoringData scoreInputFile
  let word = "ABC"
      expectedScore = 6
      actualScore = getWordScore word scores
  putStrLn $ "Check word score for '" ++ word ++ "' is 6: " ++ if actualScore == expectedScore then "Pass" else "Fail, expected " ++ show expectedScore ++ " but got " ++ show actualScore

testGetWordScoreABAM :: FilePath -> IO ()
testGetWordScoreABAM scoreInputFile = do
  scores <- getScoringData scoreInputFile
  let word = "ABAM"
      expectedScore = 95
      actualScore = getWordScore word scores
  putStrLn $ "Check word score for '" ++ word ++ "' is 95: " ++ if actualScore == expectedScore then "Pass" else "Fail, expected " ++ show expectedScore ++ " but got " ++ show actualScore

testGetWordScoreABNOP :: FilePath -> IO ()
testGetWordScoreABNOP scoreInputFile = do
  scores <- getScoringData scoreInputFile
  let word = "ABNOP"
      expectedScore = 234
      actualScore = getWordScore word scores
  putStrLn $ "Check word score for '" ++ word ++ "' is 234: " ++ if actualScore == expectedScore then "Pass" else "Fail, expected " ++ show expectedScore ++ " but got " ++ show actualScore