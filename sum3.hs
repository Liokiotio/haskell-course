groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems xs = groupElems2 $ groupElems1 xs

groupElems1 :: Eq a => [a] -> [[a]]
groupElems1 (x:xs) = [x] : groupElems xs
groupElems1 [] = []

groupElems2 :: Eq a => [[a]] -> [[a]]
groupElems2 [] = [[]]
groupElems2 (xs:[]) = [xs]
groupElems2 (xs:[]:[]) = [xs]
groupElems2 (xs:ys:zs) | head xs == head ys = groupElems2 ((xs ++ ys):zs)
                       | otherwise = xs : groupElems2 (ys:zs)