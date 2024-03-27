module Main where

main :: IO ()
-- main = putStrLn "Hello, Haskell!"
main = do 
  let input = "Dictionaries/Dictionary.txt"   -- dictionary
  contents <- readFile input
  putStrLn $ "\n" ++ show (length (lines contents)) ++ " lines \n"
  -- print the first 10 lines
  mapM_ putStrLn $ take 10 $ lines contents
  
  {-
    Function to check if a word exists in the dictionary.
      - Takes a String as input 
      - Takes a String for the dictionary name (since we might have multiple)
      - Returns the String if it exists
      - Returns null if it isn't in the dictionary
    
    We can probably change this to return a bool later, I think returning the word will make troubleshooting 
    easier at first. 
    
    findWord <SEARCH-TERM> -> <DICTIONARY-FILE-NAME> -> <RETURN>
  -}
  -- findWord :: [Char] -> [Char] -> [Char]
  -- findWord x y = undefined