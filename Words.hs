-- Solver for countdown word games

module Words(
    getWords
  , dictWords
  , noWords
  , vowels
  , consonant
)where

import Control.Applicative
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

getWords :: FilePath -> IO [String]
getWords path = do
    contents <- readFile path
    return (lines contents)

dictWords :: IO [String]
dictWords = getWords "/usr/share/dict/words"

-- The number of words in the dictionary
noWords :: IO Int
noWords = liftM length $ dictWords

vowels :: [Char]
vowels = ['a', 'e', 'i', 'o', 'u']

consonant :: [Char]
consonant =
    foldr (\x xs -> if x `elem` vowels
                    then xs else x:xs) [] letters
    where letters = ['a'..'z']

-- Prefix Trees / Trie
data Trie a = Trie (Map a (Trie a)) Bool
  deriving (Show)

emptyTrie    :: Ord a => Trie a
emptyTrie = Trie Map.empty False

insert :: Ord a => [a] -> Trie a -> Trie a
insert [] (Trie m _) = Trie m True
insert (c : w) (Trie m b) =
    case Map.lookup c m of
        Nothing -> insert (c : w) $ Trie (Map.insert c emptyTrie m) b
        Just tr -> Trie (Map.insert c (insert w tr) m) b

find :: Ord a => [a] -> Trie a -> Bool
find []      (Trie _ b) = b
find (c : w) (Trie m _) =
    maybe False (find w) $ Map.lookup c m

-- Word completion helper
complete :: Ord a => [a] -> Trie a -> [[a]]
complete [] (Trie m b) = [[] | b] ++ concat [map (c :) (complete [] tr) | (c, tr) <- Map.toList m]
complete (c : w) (Trie m _) =
    maybe [] (map (c :) . complete w) $ Map.lookup c m

-- Build a large Trie from a sequence of strings (i.e our dict)
foldTrie :: Ord a => [[a]] -> Trie a
foldTrie [] = emptyTrie
foldTrie xs = foldr (\y ys -> insert y ys) emptyTrie xs

-- Builds the Trie from our dictionary
buildDictTrie :: IO (Trie Char)
buildDictTrie = (liftM foldTrie) $ dictWords
