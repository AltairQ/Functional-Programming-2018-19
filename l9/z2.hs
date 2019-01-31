primes :: [Integer]

primes = 2:[x | x <- [3..],
            all (\n-> (x `mod` n)/=0) $
            takeWhile (\n-> (n*n) <= x) primes ]