change :: (Ord a, Num a) => a -> [[a]]
change ch | ch>0 = [c:cs | c <- coins, (ch - c) >= 0, cs <- change (ch - c)]
          | otherwise = [[]]

coins:: (Ord a, Num a) => [a]
coins = [2,3,7]