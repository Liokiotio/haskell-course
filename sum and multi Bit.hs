data Bit = Zero | One deriving (Eq, Show)
data Sign = Minus | Plus deriving (Eq, Show)
data Z = Z Sign [Bit] deriving (Eq, Show)

add :: Z -> Z -> Z
add (Z s1 x) (Z s2 y) = helper3 (helper2 s1 (helper1 0 x) + helper2 s2 (helper1 0 y))

mul :: Z -> Z -> Z
mul (Z s1 x) (Z s2 y) = helper3 (helper2 s1 (helper1 0 x) * helper2 s2 (helper1 0 y))

helper1 :: Integer -> [Bit] -> [Integer]
helper1 z [] = []
helper1 z (x:xs) | x == Zero = 0 : helper1 (z+1) xs
                 | x == One =  2^z `seq` 2^z : helper1 (z+1) xs

helper2 :: Sign -> [Integer] -> Integer
helper2 b a | b == Minus = (-1) * foldl' (+) 0 a 
                  | b == Plus = foldl' (+) 0 a 

helper3 :: Integer -> Z
helper3 i | i >= 0 = Z Plus a
          | i <  0 = Z Minus a
           where
            a =  f (abs i) 
            f x | x == 1 = [One]
                | x `mod` 2 == 0 = Zero : f (x `div` 2)
                | x `mod` 2 == 1 = One : f (x `div` 2)

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f z []     = z
foldl' f z (x:xs) = let z' = z `f` x 
                          in seq z' $ foldl' f z' xs