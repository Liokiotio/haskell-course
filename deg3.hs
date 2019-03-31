sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x < 0 = sum'n'count (x * (-1))
              | x == 0 = (0, 1)
              | x > 0 = divv x 0 1

divv :: Integer -> Integer -> Integer -> (Integer, Integer)
divv x a n | x `div` 10 >= 1 = divv (did x) (a + ost x) (n + 1)
           | x `div` 10 < 1 = (x + a, n)
     
         where   
         did x = x `div` 10
         ost x = x `mod` 10
     
