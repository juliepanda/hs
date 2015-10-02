lastDigit       :: Integer -> Integer
lastDigit x = read xs :: Integer
    where xs = [last (show x)]

dropLastDigit   :: Integer -> Integer
dropLastDigit x = read xs :: Integer
    where xs = init (show x)

toRevDigits :: Integer -> [Integer]
toRevDigits 0 = []
toRevDigits x
    | x < 0 = []
    | x > 0 = map read $ words (reverse (show x)) :: [Integer]
