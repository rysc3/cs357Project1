module Tests(
  runAllTests
) where

  {-
    Testing everything is still working, if we run this before and after staging our merges to main our lives should be easier. 

    and I want to make a git pipeline work but git doesn't love haskell.
  -}
import Score (getScoringData, getLetterScore)
-- import Dictionary (getListOfStrings)

-- Function to run all tests
runAllTests :: FilePath -> FilePath -> IO ()
runAllTests dictionaryInputFile scoreInputFile = do
  putStrLn "---------- Running All Tests ------------"

  putStrLn "-- Dictionary --"
  putStrLn "(readFile)"
  putStrLn "-----"
  testReadDictionary dictionaryInputFile
  putStrLn "(getListOfStrings)"
  -- testGetListOfStrings dictionaryInputFile
  putStrLn "----------------"


  putStrLn "-- Score --"
  putStrLn "(getScoringData)"
  putStrLn "-----------"
  testReadScoringData scoreInputFile
  testGetLetterScore scoreInputFile 'A'
  testGetLetterScore scoreInputFile 'B'
  testGetLetterScore scoreInputFile 'C'
  putStrLn "-----------"


  putStrLn "---------- getListOfStrings ------------"
  -- testGetListOfStrings dictionaryInputFile


testReadDictionary :: FilePath -> IO ()
testReadDictionary dictionaryInputFile = do
  contents <- readFile dictionaryInputFile
  putStrLn $ "\n" ++ show (length (lines contents)) ++ " lines"
  mapM_ putStrLn $ take 10 $ lines contents


testReadScoringData :: FilePath -> IO ()
testReadScoringData scoreInputFile = do
  scores <- getScoringData scoreInputFile
  putStrLn $ show scores


testGetLetterScore :: FilePath -> Char -> IO ()
testGetLetterScore scoreInputFile letter = do
  scores <- getScoringData scoreInputFile
  putStrLn $ " -- " ++ [letter] ++ " --"
  putStrLn $ show $ getLetterScore letter scores


-- testGetListOfStrings :: FilePath -> IO ()
-- testGetListOfStrings dictionaryInputFile = do
--   contents <- readFile dictionaryInputFile
--   putStrLn $ head (getListOfStrings $ take 50 contents)