{--

Find solutions to the Countdown game.
Based on Graham Huttons paper and lectures

--}

module Main
where

import Data.Maybe
import Data.List
import Data.Function(on)
import Data.Ord(comparing)

import System.Random

-- Rules

-- Intermediate results must be positive natural numbers

-------------------------------------------------------------------------------

type Target = Int
type Choices = [Int]

-- TODO shuffle numbers so they get selected randomly

-- Select numbers n being the number of big numbers to take
-- n must be less than 4 so encode this in the logic here TODO!
getNumbers :: Int -> [Int]
getNumbers n = take n large ++ take (6 - n) small
  where large = [25,50,75,100]
        small = concat $ replicate 2 [1..10]

-- Create a new countdown problem
-- generateProblem :: (Choices, Target)
-- generateProblem =

-------------------------------------------------------------------------------

groupSubsByLen :: [[a]] -> [[[a]]]
groupSubsByLen xs = groupBy ((==) `on` length) $ sortBy (compare `on` length) xs

sortByLen :: [[a]] -> [[a]]
sortByLen = sortBy (comparing length)

-- Sort into sublists (there is a native version subsequences)
sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = subs
  where subs = sublists xs ++ [x:sublist | sublist <- sublists xs]

-- What are all the ways in which we can combine the elements in the list
subsequencePermutations :: [a] -> [[a]]
subsequencePermutations [] = []
subsequencePermutations xs@(h:t) = [y | x <- sublists xs, y <- permutations x]

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort left ++ [x] ++ (qsort right)
  where left  = [y | y <- xs, y <= x]
        right = [y | y <- xs, y > x]

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

-- An expression can fail if it falls outside of our initial constraints
-- i.e if it is zero or a fraction etc
eval :: Expr -> [Int]
eval (Val n)    = [n | n > 0 ]
eval (Ap o e1 e2) = [applyOp o x y | x <- eval e1
                                   , y <- eval e2
                                   , valid o x y]

-- Same thing using Maybe type and do notation (Exercise)
-- maybeEval :: Expr -> Maybe Int
-- maybeEval (Val n) = if (n > 0) then Just n else Nothing

-- All possible ways to split a list into two non empty parts
-- splitNums xs =

