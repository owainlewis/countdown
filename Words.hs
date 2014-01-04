-- Solver for countdown word games

module Words(
    solve
  , bestWord
) where

import Data.Char ( toLower, isSpace )
import Control.Applicative
import Control.Monad
import Data.Map ( Map )
import qualified Data.Map as Map
import System.Random ( randomRIO )
import Data.List (tails, nub, maximumBy)
import Data.Function (on)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1]
                                  , x <- combinations (n-1) (drop (i+1) xs) ]

-- All ways to combine the letters in a sequence
-- TODO check this with some tests and make it faster
allPerms :: [a] -> [[a]]
allPerms xs = concat [f y xs | y <- [2..(length xs)]]
    where f n w = concatMap permutations $ combinations n w

-- Load the dictionary and clean it by trimming and normalizing to lowercase
getWords :: FilePath -> IO [String]
getWords path = do
    contents <- readFile path
    let clean = map (strLower . trim) $ lines contents
    return $ clean
    where strLower = map toLower

dictWords :: IO [String]
dictWords = getWords "wrds.txt"

-- The number of words in the dictionary
noWords :: IO Int
noWords = liftM length $ dictWords

containsWord wrd = do
    allWords <- dictWords
    let r = wrd `elem` allWords
    return r

data Game = Game [String] deriving ( Show )

randomFromList :: [a] -> IO a
randomFromList xs =
    randomRIO (0, length xs - 1) >>= return . (xs !!)

vowel :: [Char]
vowel = ['a', 'e', 'i', 'o', 'u']

consonant :: [Char]
consonant =
    foldr (\x xs -> if x `elem` vowel
                    then xs else x:xs) [] letters
    where letters = ['a'..'z']

infStream :: [a] -> [IO a]
infStream f = [randomFromList f | x <- [1..]]

getNRandom :: Int -> [a] -> IO [a]
getNRandom n xs = sequence $ replicate n $ randomFromList xs

getRandomGame :: IO String
getRandomGame = do
    v <- getNRandom 3 vowel
    c <- getNRandom 6 consonant
    let result = v ++ c
    return result

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

-- Permutations of a given length
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = [ y:zs | (y,ys) <- select xs, zs <- permutations ys]
    where select []     = []
          select (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- select xs ]

-- There is an idiomatic way to do this with flatmap like Scala but I'm can't remember what it is
fromMaybeList :: [Maybe a] -> [a]
fromMaybeList []     = []
fromMaybeList (x:xs) = case x of
    Just v -> v : fromMaybeList xs
    Nothing -> fromMaybeList xs

-- This works brute force but isn't making use of the Trie as of yet and is therefore
-- fairly slow
solve :: String -> IO [String]
solve letters = do
    trie <- buildDictTrie
    let result = map (\p -> if (find p trie) then Just p else Nothing) $ allPerms letters
    return $ fromMaybeList result

-- Find the best solution word in the countdown words round
-- This is fairly slow because we're not using the Trie properly but fast enough to
-- be classed as working at this point
bestWord :: String -> IO [Char]
bestWord letters =
    liftM (maxWord . nub) $ solve letters
    where maxWord = maximumBy (compare `on` length)

-- bestWord "ojonased" >> "anodes"
test = "ojonased"

-- END
