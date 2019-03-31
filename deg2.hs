sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x > 99 || x < (-99) = error "need 2 digits"
              | x < 0 = sum'n'count (x * (-1))
              | x == 0 = (0, 1)
              | x > 0 = divv x

divv :: Integer -> (Integer, Integer)
divv x | x `div` 10 >= 1 = (x1, 2) 
       | x `div` 10 < 1 = (x, 1)
     
         where   
         did x = x `div` 10
         ost x = x `mod` 10
         x1 = did x + ost x
     
