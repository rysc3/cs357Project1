module Tests where

  {-
    Testing everything is still working, if we run this before and after staging our merges to main our lives should be easier. 

    and I want to make a git pipeline work but git doesn't love haskell.
  -}
import Score (getScoringData, getLetterScore)
import Dictionary (getListOfStrings)

-- Define your test functions here

-- Test reading input from dictionary
testReadDictionary :: FilePath -> IO ()
testReadDictionary dictionaryInputFile = do
  contents <- readFile dictionaryInputFile
  putStrLn $ "\n" ++ show (length (lines contents)) ++ " lines \n"
  mapM_ putStrLn $ take 10 $ lines contents

-- Test reading scoring data
testReadScoringData :: FilePath -> IO ()
testReadScoringData scoreInputFile = do
  scores <- getScoringData scoreInputFile
  putStrLn $ show scores

-- Test getting letter scores
testGetLetterScore :: FilePath -> Char -> IO ()
testGetLetterScore scoreInputFile letter = do
  scores <- getScoringData scoreInputFile
  putStrLn $ " -- " ++ [letter] ++ " --"
  putStrLn $ show $ getLetterScore letter scores

-- Test getListOfStrings function
testGetListOfStrings :: FilePath -> IO ()
testGetListOfStrings dictionaryInputFile = do
  contents <- readFile dictionaryInputFile
  putStrLn $ head (getListOfStrings $ take 50 contents)

-- Function to run all tests
runAllTests :: FilePath -> FilePath -> IO ()
runAllTests dictionaryInputFile scoreInputFile = do
  putStrLn "---------- Running All Tests ------------"
  putStrLn "---------- Dictionary ------------"
  testReadDictionary dictionaryInputFile

  putStrLn "---------- Score ------------"
  testReadScoringData scoreInputFile
  testGetLetterScore scoreInputFile 'A'
  testGetLetterScore scoreInputFile 'B'
  testGetLetterScore scoreInputFile 'C'

  putStrLn "---------- getListOfStrings ------------"
  testGetListOfStrings dictionaryInputFile