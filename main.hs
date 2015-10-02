doubleMe x = x + x
doubleUs' x y = doubleMe x + doubleMe y
doubleSmallNumber' x = if x > 100 then x else x*2
boomBang' xs = [ if x < 10 then "BOOM" else "BANG" | x <- xs, odd x]
length' xs = sum [1 | _ <- xs]

removeUpper :: [Char] -> [Char]
removeUpper st = [c | c <- st, c `elem` ['A'..'X']]

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs


