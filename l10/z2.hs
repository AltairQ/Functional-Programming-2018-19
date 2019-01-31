data BTree a = Leaf | Node (BTree a) a (BTree a) deriving Show

data Array a = Array
    { tree :: BTree a,
      emax :: Integer
    } deriving Show

bst_search :: BTree a -> Integer -> a
bst_search Leaf _ = undefined
bst_search (Node lt x rt) n =
    if n == 1 then x else 
        if (n `mod` 2 == 0) then
            bst_search lt (quot n 2)
        else
            bst_search rt (quot n 2)

aempty :: Array a
aempty = Array {tree = Leaf, emax = 0}

asub :: Array a -> Integer -> a
asub arr n = 
    if (emax arr < n) || (n < 1)
    then 
        undefined
    else
        bst_search (tree arr) n


bst_update :: BTree a -> Integer -> a -> BTree a
bst_update Leaf _ _ = undefined
bst_update (Node lt x rt) n nx =
    if n == 1 then
        (Node lt nx rt) else 
        if (n `mod` 2 == 0) then
            Node (bst_update lt (quot n 2) nx) x rt
        else
            Node lt x (bst_update rt (quot n 2) nx) 

aupdate :: Array a -> Integer -> a -> Array a
aupdate arr n x =
    if (emax arr < n) || (n < 1)
    then 
        undefined
    else
        arr { tree = bst_update (tree arr) n x}


bst_append :: BTree a -> Integer -> a -> BTree a
bst_append Leaf 1 x = Node Leaf x Leaf
bst_append (Node lt a rt) n x =
    if (n `mod` 2 == 0) then
        Node (bst_append lt (quot n 2) x) a rt 
    else
        Node lt a (bst_append rt (quot n 2) x)

ahiext :: Array a -> a -> Array a
ahiext arr x =
    arr {tree = bst_append (tree arr) nmax x, emax = nmax }
    where
        nmax = 1 + emax arr

bst_pop :: BTree a -> Integer -> BTree a
bst_pop Leaf _ = undefined
bst_pop _ 1 = Leaf
bst_pop (Node lt a rt) n =
    if (n `mod` 2 == 0) then
        Node (bst_pop lt (quot n 2) ) a rt 
    else
        Node lt a (bst_pop rt (quot n 2) )

ahirem :: Array a -> Array a
ahirem arr =
    if (emax arr < 1) then undefined else
        arr {tree = bst_pop (tree arr) cmax, emax = cmax-1 }
        where
            cmax = emax arr