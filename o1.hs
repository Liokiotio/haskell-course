oddsOnly :: Integral a => [a] -> [a]
oddsOnly xs = o1 xs []

o1 (x : xs) z | odd x == True = if null xs == True then z ++ (x : []) else o1 xs (x : z)
              | even x == True =  if null xs == True then z else o1 xs z

