data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height = helper 0

helper :: Int -> Tree a -> Int
helper h (Leaf _) = h
helper h (Node x y) = max (helper (h+1) x) (helper (h+1) y)

size :: Tree a -> Int
size (Leaf _) = 0
size (Node z y) = 1 + size z + size y