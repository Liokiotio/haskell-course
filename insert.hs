perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concatMap (insert x) (perms xs)

insert :: a -> [a] -> [[a]]
insert x [] = [[x]]
insert x z@(y:ys) = (x:z) : map (y:) (insert x ys) 

-- 1 [2,3,4] = [[1,2,3,4], [2,1,3,4], [2,3,1,4], [2,3,4,1]]