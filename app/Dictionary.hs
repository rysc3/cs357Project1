module Dictionary(
  Trie,
  buildDictionary,
  contains,
  shrinkTrie,
  countWords
) where

import qualified Data.Map.Strict as M --need a map because we don't know how many children each node is going to have
import Data.Text (pack, unpack, splitOn)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

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

buildDictionary :: FilePath -> IO Trie
buildDictionary filePath = do
    contents <- readFileText filePath
    let wordList = getListOfStrings contents
    return $ foldr insert emptyTrie wordList

-- AIsolver
--getListOfStrings :: String -> [String]
--getListOfStrings s = map unpack (splitOn (pack "\r\n") (pack s)) --convert string to text, split on \r\n, convert back to string




shrinkTrie :: [Char] -> Trie -> Trie
shrinkTrie letters trie = shrinkTrie' letters 0 trie
  where
    shrinkTrie' :: [Char] -> Int -> Trie -> Trie
    shrinkTrie' _ _ (Node True _) = Node True M.empty 
    -- Keep valid words
    shrinkTrie' _ 7 _             = Node False M.empty 
    -- Max depth reached, prune
    shrinkTrie' [] _ _            = Node False M.empty 
    -- Ran out of letters, prune
    shrinkTrie' (x:xs) depth (Node end chMap) =
      case M.lookup x chMap of
        Just child ->
          let nextDepth = if end then depth + 1 else depth 
          -- Increase depth only if reaching end of word
          in Node end (M.singleton x (shrinkTrie' xs nextDepth child)) 
          -- Recurse down the trie
        Nothing ->
          Node False M.empty -- Letter not found, prune


countWords :: Trie -> Int
countWords trie = countWords' trie
  where
    countWords' :: Trie -> Int
    countWords' (Node True childrenMap) = 1 + sum (map countWords' (M.elems childrenMap))
    countWords' (Node False childrenMap) = sum (map countWords' (M.elems childrenMap))

getListOfStrings :: T.Text -> [String]
getListOfStrings s = map T.unpack (T.splitOn charsToText s)
  where 
    charsToText = T.pack "\r\n"   -- these need to be Text

readFileText :: FilePath -> IO T.Text
readFileText filePath = TIO.readFile filePath

