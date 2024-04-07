module Dictionary(
  search,
  buildDictionary
) where

import qualified Data.Map.Strict as M --need a map because we don't know how many children each node is going to have 


data Trie = Node (M.Map Char Trie) 
  deriving (Eq, Show)

getListOfStrings :: String -> [String]
getListOfStrings = undefined


insert :: String -> Trie -> Trie
insert = undefined


search :: String -> Bool
search =  undefined 

buildDictionary :: [String] -> Trie
buildDictionary = undefined