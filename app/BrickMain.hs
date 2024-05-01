module BrickMain where

import Options.Applicative
import System.Exit (exitSuccess)
import Control.Monad (when)


data Opts = Opts
  { printHello :: Bool
  }

opts :: Parser Opts
opts = Opts
  <$> switch
    (  long "hello"
    <> short 'h'
    <> help "Print 'Hello, World!' and exit" )

fullopts :: ParserInfo Opts
fullopts = info (helper <*> opts)
  (  fullDesc
  <> header "Hello World Program" )

main :: IO ()
main = do
  (Opts ph) <- execParser fullopts
  when ph (putStrLn "Hello, World!" >> exitSuccess)
