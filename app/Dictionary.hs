module Dictionary(
  Trie,
  buildDictionary,
  contains,
  getListOfStrings
) where

import qualified Data.Map.Strict as M --need a map because we don't know how many children each node is going to have
import Data.Text (pack, unpack, splitOn) 

-- maisy was here

data Trie = Node {
  endOfWord :: Bool,
  children :: M.Map Char Trie
}
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
emptyTrie :: Trie
emptyTrie = Node False M.empty

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

buildDictionary :: [String] -> Trie
buildDictionary s = foldr insert emptyTrie s
--string will have all words in dictionary, which we will split into a list of strings, then insert each string into the trie

getListOfStrings :: String -> [String]
getListOfStrings s = map unpack (splitOn (pack "\r\n") (pack s)) --convert string to text, split on \r\n, convert back to string