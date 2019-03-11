-- 0 1 1 2 3 5 8 13 21 ...
-- ... −21 13 −8 5 −3 2 −1 1
fibonacci :: Integer -> Integer
fibonacci n | n > 0 = helper 0 1 n
            | n < 0 = helperminus 1 (-1) n
            | n == 0 = 0 
            | otherwise = undefined

helper :: Integer -> Integer -> Integer -> Integer
helper pre sumf 1 = sumf
helper pre sumf n = helper (sumf) (pre + sumf) (n - 1)

helperminus :: Integer -> Integer -> Integer -> Integer
helperminus pre sumf (-1) = pre
helperminus pre sumf (-2) = sumf
helperminus pre sumf n = helperminus (sumf) (pre - sumf) (n + 1)
