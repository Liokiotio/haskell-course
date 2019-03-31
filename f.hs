perms :: Eq a => [a] -> [a]
perms [] = []
perms (x:xs) = x : concatMap (\x -> x : xs) xs