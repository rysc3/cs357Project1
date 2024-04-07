module Dictionary(
  search,
  buildDictionary
) where

import qualified Data.Map.Strict as M --need a map because we don't know how many children each node is going to have 


data Trie = Node (M.Map Char Trie) 
  deriving (Eq, Show)

getListOfStrings :: String -> [String]
getListOfStrings = undefined
empty :: Trie
empty = Empty M.empty

getChildren:: Trie -> M.Map Char Trie
getChildren (Node _ m) = m
getChildren (Empty m) = m

setChildren :: Trie -> M.Map Char Trie -> Trie
setChildren (Node c _) children = Node c children
setChildren (Empty _) children = Empty children  

{-
  Input from the dictionary file is of the type:
  ghci> take 75 contents
"aa\r\naah\r\naahed\r\naahing\r\naahs\r\naal\r\naalii\r\naaliis..."

  Map across all of the input string, and split every time we have a newline, in this case, `\r\n`

  Import Data.Text to help here
  pack to convet string to Text 
  splitOn to split into a list of strings 
  map unpack over list to convert back to strings
-}
getListOfStrings :: [Char] -> [[Char]]
getListOfStrings input = map unpack $ splitOn (pack "\r\n") (pack input)


insert :: String -> Trie -> Trie
insert = undefined


search :: String -> Bool
search =  undefined 

buildDictionary :: [String] -> Trie
buildDictionary = undefined