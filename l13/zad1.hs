import Prelude hiding ((++), head, tail, length, null, (!!))
import qualified Prelude ((++), head, tail, length, null, (!!))

class List l where 
    nil :: l a
    cons     :: a -> l a -> l a
    head     :: l a -> a
    tail     :: l a -> l a
    (++)     :: l a -> l a -> l a
    (!!)     :: l a -> Int -> a
    toList   :: [a] -> l a
    fromList :: l a -> [a]


instance List [] where
    nil = []
    cons = (:)
    head = Prelude.head
    tail = Prelude.tail
    (++) = (Prelude.++)
    (!!) = (Prelude.!!)
    toList = id
    fromList = id

class List l => SizedList l where 
    length :: l a -> Int
    null :: l a -> Bool
    null l = length l == 0


instance SizedList [] where
    length = Prelude.length
    null [] = True
    null _ = False


data SL l a = SL { len :: Int, list :: l a}

instance List l => List (SL l) where
    nil = SL {len = 0, list = nil}

    cons a bsl =
        SL {
        len = 1 + (len bsl),
        list = cons a (list bsl)
        }

    head = head.list

    tail asl = SL {len = (len asl)-1, list = (tail . list)  asl}

    (++) asl bsl =
        SL {
        len = (len asl) + (len bsl),
        list = (list asl) ++ (list bsl)
    }

    (!!) asl n = (!!) (list asl) n

    toList as = SL {len = length as, list = toList as}
    fromList = fromList . list 

instance List l => SizedList (SL l) where
    length = len


infixr 6 :+
data AppList a = Nil | Sngl a | AppList a :+ AppList a


apfoldr f g a apl = case apl of
    Nil -> a
    Sngl x -> g x
    (la) :+ (ra) -> f (apfoldr f g a la) (apfoldr f g a ra)

instance List AppList where
    nil = Nil
    cons a aal = (Sngl a) :+ (aal)

    head aal = case aal of
        Nil -> undefined
        Sngl x -> x
        (left):+(_) -> head left

    tail aal = case aal of
        Nil -> undefined
        Sngl _ -> Nil
        (Sngl _) :+ rest -> rest
        left :+ rest -> (tail left) :+ rest

    (++) l r = case l of
        Nil -> r
        _ -> l :+ r

    (!!) aal n = head (iterate tail aal !! n)

    toList = foldr f Nil where
        f a b = (Sngl a) :+ b

    fromList = apfoldr (Prelude.++) (return) []

instance Show a => Show (AppList a) where
    show apl = show (fromList apl)

instance SizedList AppList where
    length = apfoldr (+) (const 1) 0



-- zad 5

newtype DiffList a = DL ([a] -> [a])

instance List DiffList where
    nil = DL (id)
    cons a (DL f) = DL (\x -> a : (f x))
    head (DL f) = (Prelude.head) (f [])
    tail (DL f) = DL (\x -> tail $ f x)
    (++) (DL f) (DL g) = DL (f . g)
    (!!) dl = (Prelude.!!) (fromList dl)
    toList xs = DL (\x -> (Prelude.++) xs x)
    fromList (DL f) = f []

instance SizedList DiffList where
    length = (Prelude.length) . (fromList)

instance Show a => Show (DiffList a) where
    show = show . fromList


-- zad 6

data RAL a = Empty | Zero (RAL (a,a)) | One a (RAL (a,a))

addOne :: RAL a -> a -> RAL a
addOne r a = case r of
    Empty -> One a (Empty)
    Zero rest -> One a rest
    One b rest -> Zero (addOne rest (a, b))

minusOne :: RAL a -> (a, RAL a)
minusOne ar = case ar of
    Empty -> undefined -- dirty
    Zero rest ->
        let ((x,y), subtree) = minusOne rest in
            (x, One y subtree)
    One a rest -> 
        case rest of 
            Empty -> (a, Empty)
            _     -> (a, Zero rest)

ralNth :: RAL a -> Int -> Int -> (a, Int)

-- WARNING COULD BE BROKEN
ralNth Empty _ _ = undefined 
ralNth (Zero rest) n csize =
    let ((x,y), cn) = ralNth rest n (csize*2) in
        if (cn `mod` csize) < hcsize then 
            (x, (cn `mod` csize))
        else
            (y, (cn `mod` csize) - hcsize)

    where
        hcsize = (csize+1) `div` 2


ralNth (One a rest) n csize =
    if n < csize then
        (a, n)
    else
        let ((x,y), cn) = ralNth rest (n-csize) (csize*2) in
            if (cn `mod` csize) < hcsize then 
                (x, (cn `mod` csize))
            else
                (y, (cn `mod` csize) - hcsize)

    where
        hcsize = (csize+1) `div` 2

-- /WARNING COULD BE BROKEN


flatten Empty = []
flatten a = h : (flatten t)
    where
        (h, t) = minusOne a


instance List RAL where
    nil = Empty
    cons = flip addOne
    head = fst . minusOne
    tail = snd . minusOne
    (++) ar br = foldr (cons) br (fromList ar)
    (!!) ar n = fst $ ralNth ar n 1
    toList ax = foldr (cons) nil ax
    fromList = flatten

instance Show a => Show (RAL a) where
    show = show . fromList

instance SizedList RAL where
    length = length . fromList
