{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RankNTypes #-}

import Prelude hiding (pred, succ)

newtype Church = Church (∀ a. (a->a)->(a->a))

instance Num Church where
    (+) (Church c1) (Church c2) = Church (c2 succ c1)
        where
            succ = \n f x -> f (n f x)

    (-) ch1 ch2 = (sub ch1 ch2)
        where
            pred (Church n) = 
                Church (\s z -> fst (n (\(_,x)->(x,s x)) (undefined,z)))
                -- not the greatest solution
            sub a (Church b) = b pred a
            

    (*) (Church c1) (Church c2) = Church (\f -> c1 (c2 f))

    abs = id -- we do not support negative numbers

    signum (Church c) = if (c (+ 1) 0) > 0 then ch_one else ch_zero
        where
            ch_zero = Church (\f x -> x  )
            ch_one  = Church (\f x -> f x)

    fromInteger n =
        Church (it n)
            where
                it k f x = if k <= 0 then x else it (k-1) f (f x)

instance Eq Church where
    (==) (Church c1) (Church c2) =
        c1 (+ 1) 0 == c2 (+ 1) 0

instance Ord Church where
    (<=) (Church c1) (Church c2) =
        c1 (+ 1) 0 <= c2 (+ 1) 0

instance Show Church where
    show (Church c) = show (c (+ 1) 0)
