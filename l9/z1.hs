f :: [Integer] -> [Integer]
f (n:ns) = [x | x <- ns, (mod x n) /=0 ]

primes :: [Integer]
primes = map (head) $ iterate f [2..]

 