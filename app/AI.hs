module AI()where

import Dictionary(search, buildDictionary)

getWords :: String -> [String]
getWords = undefined --takes a string of letters in anagram, trie containing dictionary,
--returns list of words that can be made from the anagram letters