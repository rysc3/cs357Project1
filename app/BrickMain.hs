module BrickMain where

import Control.Monad (when)
import Options.Applicative
import System.Exit (exitSuccess)

data Opts = Opts
  { printHello :: Bool
  }

opts :: Parser Opts
opts =
  Opts
    <$> switch
      ( long "hello"
          <> short 'h'
          <> help "Print 'Hello, World!' and exit"
      )

fullopts :: ParserInfo Opts
fullopts =
  info
    (helper <*> opts)
    ( fullDesc
        <> header "Hello World Program"
    )

main :: IO ()
main = do
  putStrLn "Type something"
  loop

loop :: IO ()
loop = do
  input <- getLine
  if input == "exit"
    then exitSuccess
    else do
      putStrLn $ "You typed: " ++ input
      loop