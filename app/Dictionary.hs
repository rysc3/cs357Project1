module Dictionary(
  Trie,
  buildDictionary,
  contains,
  getListOfStrings,
  shrinkTrie,
  countWords,
  printTrie
) where


import qualified Data.Map.Strict as M --need a map because we don't know how many children each node is going to have
import Data.Char (toUpper)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.List (nub)


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
insert [] trie = trie { endOfWord = True }
insert word trie
    | hasDuplicates word = trie  -- Skip words with duplicates
    | otherwise = insert' (map toUpper word) trie
  where
    insert' :: String -> Trie -> Trie
    insert' [] node = node { endOfWord = True }
    insert' (x:xs) node =
        let childNode = M.findWithDefault emptyTrie (toUpper x) (children node)
            newNode = insert' (map toUpper xs) childNode
            updatedChildren = M.insert (toUpper x) newNode (children node)
        in node { children = updatedChildren }

    -- Function to check if a word has duplicate letters
    hasDuplicates :: String -> Bool
    hasDuplicates word = length word /= length (nub word)


contains :: String -> Trie -> Bool
contains [] trie = endOfWord trie
contains (x:xs) trie = maybe False (contains (map toUpper xs)) (M.lookup (toUpper x) (children trie)) --need to handle for nothing or Just cases when called 


buildDictionary :: FilePath -> IO Trie
buildDictionary filePath = do
    contents <- readFileText filePath
    let wordList = getListOfStrings contents
    return $ foldr insert emptyTrie wordList

-- AIsolver
--getListOfStrings :: String -> [String]
--getListOfStrings s = map unpack (splitOn (pack "\r\n") (pack s)) --convert string to text, split on \r\n, convert back to string

getListOfStrings :: T.Text -> [String]
getListOfStrings s = map T.unpack (T.splitOn charsToText s)
  where 
    charsToText = T.pack "\r\n"   -- these need to be Text


readFileText :: FilePath -> IO T.Text
readFileText filePath = TIO.readFile filePath


countWords :: Trie -> Int
countWords trie =  ( countWords' trie) -1
  where
    countWords' :: Trie -> Int
    countWords' (Node True childrenMap) = 1 + sum (map countWords' (M.elems childrenMap))
    countWords' (Node False childrenMap) = sum (map countWords' (M.elems childrenMap))


printTrie :: Trie -> IO ()
printTrie = printTrie' ""


printTrie' :: String -> Trie -> IO ()
printTrie' prefix (Node endOfWord children) = do
  let prefix' = if endOfWord then prefix else ""
  if endOfWord
    then putStrLn prefix'
    else return ()
  mapM_ (\(char, child) -> printTrie' (prefix ++ [char]) child) (M.toList children)


shrinkTrie :: [Char] -> Trie -> Trie
shrinkTrie letters trie = limitDupes (map toUpper letters) $ chopDepth (pruneBadBranch trie letters)


pruneBadBranch :: Trie -> [Char] -> Trie
pruneBadBranch trie chars = trie { children = prunedChildren }
  where
    prunedChildren = M.map (\child -> pruneBadBranch child (map toUpper chars)) 
        $ M.filterWithKey (\k _ -> k `elem` (map toUpper chars)) (children trie)


chopDepth :: Trie -> Trie
chopDepth trie = chopOffAtDepth' trie 0
  where
    chopOffAtDepth' :: Trie -> Int -> Trie
    chopOffAtDepth' t depth
      | depth == 7 = t { children = M.empty }  -- Remove all children if depth is 7
      | otherwise = t { children = prunedChildren }
      where
        prunedChildren = M.map (\child -> chopOffAtDepth' child (depth + 1)) (children t)

limitDupes :: [Char] -> Trie -> Trie
limitDupes _ t = id t