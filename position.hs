evenOnly :: [a] -> [a]
evenOnly = snd . foldl (\(y,z) x -> if even (y+1) then (y+1,z++[x]) else (y+1, z)) (0,[])

evenOnly' :: [a] -> [a]
evenOnly' = snd . foldr (\a (xs, ys) -> (a : ys, xs)) ([], [])