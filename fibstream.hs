fib :: Int -> [Int] -> [Int]
fib x ys = x : fib (x + head ys) (x:ys)



-- fib' x = z : x : fib' (x+fib')