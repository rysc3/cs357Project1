module Dictionary(
  Trie,
  buildDictionary,
  contains,
  getListOfStrings
) where

import qualified Data.Map.Strict as M
import Data.Text (pack, splitOn, unpack, toUpper)

data Trie = Node
  { endOfWord :: Bool,
    children :: M.Map Char Trie
  }
  deriving (Eq)

-- Make tree show pretty
instance Show Trie where
  show trie = unlines $ map (showPath "") (M.toList (children trie))
    where
      showPath prefix (char, child) =
        let newPath = prefix ++ [char]
            indentation = if null prefix then "" else "    "
            endMarker = if endOfWord child then " -> (END)" else ""
        in indentation ++ newPath ++ endMarker ++ "\n" ++ showPath' newPath child
      showPath' prefix (Node _ children') = unlines $ map (showPath prefix) (M.toList children')

emptyTrie :: Trie
emptyTrie = Node False M.empty

{-
  TODO @here 
  We need to convert every letter to be CAPITALIZED as we add it to the tree. Right now contains is case sensitive and returns false for a word if it has any capitalizations. 
    
    In the scoring data I have everything working by converting to be capitalized firsrt, so we should do the same thing here to keep it all uniform.
-}
insert :: String -> Trie -> Trie
insert [] trie = trie {endOfWord = True}
insert (x : xs) trie =
  let childNode = M.lookup x (children trie)
      newNode = insert xs (maybe emptyTrie id childNode)
      updatedChildren = M.insert x newNode (children trie)
   in trie {children = updatedChildren}

contains :: String -> Trie -> Bool
contains [] trie = endOfWord trie
contains (x : xs) trie = maybe False (contains xs) (M.lookup x (children trie))

buildDictionary :: String -> Trie
buildDictionary s = foldr insert emptyTrie (getListOfStrings s)

{-
  Dictionary file is read in in the format "word\r\nword\r\nword\r\n", this function 
  converts into a list of the actual words.
-}
getListOfStrings :: String -> [String]
getListOfStrings s = map unpack (splitOn (pack "\r\n") (pack s))