{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit       :: Integer -> Integer
lastDigit x = read xs :: Integer
    where xs = [last (show x)]

-- Drop the last digit from a number
dropLastDigit   :: Integer -> Integer
dropLastDigit x = read xs :: Integer
    where xs = init (show x)

-- Exercise 2 -----------------------------------------
toRevDigits     :: Integer -> [Integer]
toRevDigits x
    | x <= 0 = []
    | x > 0 =  x `mod` 10 : toRevDigits (x `div` 10)

-- Exercise 3 -----------------------------------------
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs:xss) = x : xs*2 : doubleEveryOther xss

-- Exercise 4 -----------------------------------------
--
-- -- Calculate the sum of all the digits in every Integer.
sumDigits       :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
    | x >= 10     = addDigs x + sumDigits xs
    | otherwise   = x + sumDigits xs

-- helper to sumDigits
addDigs         :: Integer -> Integer
addDigs 0 = 0
addDigs x 
    | x >= 10     = x `mod` 10 + addDigs (x `div` 10)
    | otherwise   = x

-- Exercise 5 -----------------------------------------
--
-- -- Validate a credit card number using the above functions.
luhn          :: Integer -> Bool
luhn 0 = False
luhn x
    | x < 0 = error "can't have negative input"
    | credify == 0  = True
    | otherwise     = False
        where credify = (sumDigits (doubleEveryOther (toRevDigits x) ) ) `mod` 10

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n == 0      = []
    | n > 1       = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a
    | n == 1      = [(a, b)]
