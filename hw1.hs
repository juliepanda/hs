lastDigit       :: Integer -> Integer
lastDigit x = read xs :: Integer
    where xs = [last (show x)]

dropLastDigit   :: Integer -> Integer
dropLastDigit x = read xs :: Integer
    where xs = init (show x)

toRevDigits     :: Integer -> [Integer]
toRevDigits 0 = []
toRevDigits x
    | x < 0 = []
    | x > 0 =  x `mod` 10 : toRevDigits (x `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs:xss) = x : xs*2 : doubleEveryOther xss


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

luhn          :: Integer -> Bool
luhn 0 = False
luhn x
    | x < 0 = error "can't have negative input"
    | credify == 0  = True
    | otherwise     = False
        where credify = (sumDigits (doubleEveryOther (toRevDigits x) ) ) `mod` 10

