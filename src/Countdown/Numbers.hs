module Countdown.Numbers
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
    where subs = sublists xs ++ rest
          rest = [x:sublist | sublist <- sublists xs]

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
valid Add  _ _ = True
valid Sub  x y = x > y
valid Mult _ _ = True
valid Div  x y = x `mod` y == 0

instance Show Op where
    show Add   = "+"
    show Sub   = "-"
    show Mult  = "*"
    show Div   = "/"

applyOp :: Op -> Int -> Int -> Int
applyOp Add  x y = x + y
applyOp Sub  x y = x - y
applyOp Mult x y = x * y
applyOp Div  x y = x `div` y

-- Recursive data type either value or application
-- 1 + 2 -> Ap Add (Val 1) (Val 2)
data Expr = Val Int | Ap Op Expr Expr

instance Show Expr where
    show (Val v) = show v
    show (Ap op x y) = let space = " " in
        show x ++ space ++ show op ++ space ++ show y

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
split' :: [a] -> [([a],[a])]
split' [] = []
split' [_] = []
split' (x:xs) = ([x],xs):[(x:y, ys)|(y,ys)<-split'(xs)]

mapCons :: a -> [[a]] -> [[a]]
mapCons v xs = map (v:) xs

interleave           :: a -> [a] -> [[a]]
interleave x []      = [[x]]
interleave x (y:ys)  = (x:y:ys) : mapCons y (interleave x ys)

permutations' :: [a] -> [[a]]
permutations' []     = [[]]
permutations' (x:xs) = concatMap (interleave x) . permutations' $ xs

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = ys ++ map (x:) ys
    where ys= subs xs

-- Formalize

-- Return a list of all possible ways of choosing zero
-- or more elements from a list:
choices :: [a] -> [[a]]
choices = (concatMap permutations') . subs

combine :: Expr -> Expr -> [Expr]
combine l r = [ Ap o l r | o <- [Mult, Add, Sub, Div] ]

-- all possible expressions given a list of Int
-- expands the search space to all possible combinations
exprs :: [Int] -> [Expr]
exprs []  = []
exprs [x] = [Val x]
exprs xs  = [ x | (ls, rs) <- split' xs,
                  l        <- exprs ls,
                  r        <- exprs rs,
                  x        <- combine l r]

