{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [] []    = 0
exactMatches [] _     = 0
exactMatches _ []     = 0
exactMatches (a:as) (b:bs)
      | a == b        = 1 + exactMatches as bs
      | otherwise     = exactMatches as bs

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
-- HOW DO I MAKE THIS BETTER?
countColors :: Code -> [Int]
countColors []        = [0, 0, 0, 0, 0, 0]
countColors x         = [length (filter (==Red) x)] ++ [length (filter (==Green) x)] ++ [length (filter (==Blue) x)] ++ [length (filter (==Yellow) x)] ++ [length (filter (==Orange) x)] ++ [length (filter (==Purple) x)]


-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches [] []         = []
matches a b           = (countColors a)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove = undefined

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent = undefined

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = undefined

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes = undefined

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
