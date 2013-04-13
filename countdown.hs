-- Find solutions to the Countdown game.
-- Based on Graham Huttons paper
module Main
where

import Data.List
import Data.Function(on)
import Data.Ord(comparing)

import System.Random

-- Rules

-- Intermediate results must be positive natural numbers

-------------------------------------------------------------------------------

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

-- Type for arithmetic operations

data Op = Add | Sub | Mult | Div

valid :: Op -> Int -> Int -> Bool
valid Add  x y = True
valid Sub  x y = x < y
valid Mult x y = True
valid Div  x y = x `mod` y == 0

applyOp :: Op -> Int -> Int -> Int
applyOp Add  x y = x + y
applyOp Sub  x y = x - y
applyOp Mult x y = x * y
applyOp Div  x y = div x y

-- Recursive data type either value or application
-- 1 + 2 -> Ap Add (Val 1) (Val 2)
data Expr = Val Int | Ap Op Expr Expr

