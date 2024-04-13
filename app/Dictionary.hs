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

emptyTrie :: Trie
emptyTrie = Node '0' False M.empty

insert :: String -> Trie -> Trie
insert []     trie = trie { endOfWord = True }
insert (x:xs) trie = trie { children = M.insertWith (\_ existing -> insert xs existing) x (insert xs emptyTrie) (children trie) }

contains :: String -> Trie -> Bool
contains []     trie = endOfWord trie
contains (x:xs) trie = case M.lookup x (children trie) of
    Nothing   -> False
    Just next -> contains xs next

buildDictionary :: String -> [String] -> Trie
buildDictionary s = foldr insert emptyTrie (strToList s)
--string will have all words in dictionary, which we will split into a list of strings, then insert each string into the trie

strToList :: String -> [String]
strToList = undefined