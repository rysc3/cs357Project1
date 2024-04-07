module Dictionary(
  search,
  buildDictionary
) where

import qualified Data.Map.Strict as M --need a map because we don't know how many children each node is going to have 


data Trie = Node Char (M.Map Char Trie) | Empty (M.Map Char Trie) --can be a node or empty 
  deriving (Eq, Show)

empty :: Trie
empty = Empty M.empty

getChildren:: Trie -> M.Map Char Trie
getChildren (Node _ m) = m
getChildren (Empty m) = m

setChildren :: Trie -> M.Map Char Trie -> Trie
setChildren (Node c _) children = Node c children
setChildren (Empty _) children = Empty children  


insert :: String -> Trie -> Trie
insert = undefined


search :: String -> Bool
search =  undefined 

buildDictionary :: String -> Trie
buildDictionary s = undefined 
--string will have all words in dictionary, which we will split into a list of strings, then insert each string into the trie

strToList :: String -> [String]
strToList = undefined