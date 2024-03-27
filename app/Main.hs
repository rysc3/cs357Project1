module Main where

main :: IO ()
-- main = putStrLn "Hello, Haskell!"
main = do 
  let input = "Dictionary.txt"   -- dictionary
  contents <- readFile input
  putStrLn $ "\n" ++ show (length (lines contents)) ++ " lines \n"
  -- print the first 10 lines
  mapM_ putStrLn $ take 10 $ lines contents
