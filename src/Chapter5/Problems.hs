module Chapter5.Problems where

primes :: [Integer]
primes = primes' 2 (dropMultiples 2 [2..]) 
         where 
             primes' p (p':ns) = p : primes' p' (dropMultiples p' ns)
             dropMultiples p = filter (\i -> i `mod` p /= 0)