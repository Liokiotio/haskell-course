-- 𝑎_(0)=1; 𝑎_(1)=2; 𝑎_(2)=3; 𝑎_(𝑘+3) = 𝑎_(𝑘+2)+𝑎_(𝑘+1)−2𝑎_(𝑘)
-- seqA 301 = 1276538859311178639666612897162414
seqA :: Integer -> Integer
seqA n | n < 0 = error "arg must be >= 0"
       | n == 0 = 1
       | n == 1 = 2
       | otherwise = let 
         a0 = 1
         a1 = 2
         a2 = 3
         seqq ak ak1 ak2 2 = ak2
         seqq ak ak1 ak2 n = seqq (ak1) (ak2) (ak2 + ak1 - 2 * ak) (n - 1)
        in seqq a0 a1 a2 n
