module Dictionary(
    search,
    buildDictionary
) where

import Data.Map as M --need a map because we don't know how many children each node is going to have 
import Distribution.Simple.Build (build)


data Trie = Node (M.Map Char Trie) 
  deriving (Eq, Show)

insert :: String -> Trie -> Trie
insert = undefined


search :: String -> Bool
search =  undefined 

buildDictionary :: [String] -> Trie
buildDictionary = undefined