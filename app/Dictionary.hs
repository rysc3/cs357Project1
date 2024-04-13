module Dictionary(
  buildDictionary,
  getListOfStrings,
  contains,
  buildDictionary
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M --need a map because we don't know how many children each node is going to have 
import Data.Text (pack, unpack, splitOn)  -- getListOfStrings


data Trie = Node
    { char :: Char 
    , endOfWord :: Bool
    , children :: Map Char Trie
    } --can be a node or empty 
  deriving (Eq, Show)

getListOfStrings :: [Char] -> [[Char]]
getListOfStrings input = map unpack $ splitOn (pack "\r\n") (pack input)

emptyTrie :: Trie
emptyTrie = Node '0' False M.empty

insert :: String -> Trie -> Trie
insert []     trie = trie { endOfWord = True }
insert (x:xs) trie =
    let childNode = M.lookup x (children trie)
        newNode = insert xs (maybe emptyTrie id childNode)
        updatedChildren = M.insert x newNode (children trie)
    in trie { children = updatedChildren }

contains :: String -> Trie -> Bool
contains [] trie = endOfWord trie
contains (x:xs) trie = maybe False (contains xs) (M.lookup x (children trie)) --need to handle for nothing or Just cases when called 

buildDictionary :: String -> Trie
buildDictionary s = foldr insert emptyTrie (getListOfStrings s)
--string will have all words in dictionary, which we will split into a list of strings, then insert each string into the trie

