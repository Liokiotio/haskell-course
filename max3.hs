max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 (x:xs) (y:ys) (z:zs) = f x y z : max3 xs ys zs

 where
 f :: Ord a => a -> a -> a -> a 
 f x y z | x >= y && x >= z = x
        | y >= x && y >= z = y
        | z >= x && z >= y = z
        
max3 _ _ _ = []