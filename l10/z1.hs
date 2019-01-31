data BTree a = Leaf | Node (BTree a) a (BTree a) deriving Show

dfa :: BTree a -> Integer -> (BTree Integer, Integer)
dfa Leaf n = (Leaf, n)
dfa (Node lt _ rt) n =
    let nn = n+1
        (resl, nl) = dfa lt nn
        (resr, nr) = dfa rt nl
    in ((Node resl nn resr), nr)


dfnum :: BTree a -> BTree Integer
dfnum bt = fst $ dfa bt 0


bfn :: ([Integer], BTree a) -> ([Integer], BTree Integer)
bfn (xs, Leaf) = (xs, Leaf)
bfn (x:xs, (Node lt _ rt)) = (x+1 : xsnn, (Node ltn x rtn) )
    where
        (xsn, ltn) = bfn (xs, lt)
        (xsnn, rtn) = bfn (xsn, rt)

bfnum :: BTree a -> BTree Integer
bfnum t = rt
    where (xs, rt) = bfn (1:xs, t)
