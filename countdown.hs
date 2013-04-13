-- Countdown

module Main
where

import Data.List
import Data.Function(on)
import Data.Ord(comparing)

import System.Random

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort left ++ [x] ++ (qsort right)
  where left  = [y | y <- xs, y <= x]
        right = [y | y <- xs, y > x]

groupSubsByLen :: [[a]] -> [[[a]]]
groupSubsByLen xs = groupBy ((==) `on` length) $ sortBy (compare `on` length) xs

sortByLen :: [[a]] -> [[a]]
sortByLen = sortBy (comparing length)

sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = subs
  where subs = sublists xs ++ [x:sublist | sublist <- sublists xs]

-- What are all the ways in which we can combine the elements in the list
subsequencePermutations :: [a] -> [[a]]
subsequencePermutations [] = []
subsequencePermutations xs@(h:t) = [y | x <- sublists xs, y <- permutations x]

-------------------------------------------------------------------------------



