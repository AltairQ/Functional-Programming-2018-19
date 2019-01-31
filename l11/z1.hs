-- Imported only for Data.List.lookup
import Data.List

ana :: (b -> Maybe (a,b)) -> b -> [a]
ana f st = case f st of 
    Nothing -> []
    Just (v, st') -> v : ana f st'

mzip :: [a]->[b]->[(a,b)]
mzip al bl = ana aux (al, bl)
    where
        aux (x:xs,y:ys) = Just ((x,y),(xs,ys))
        aux _ = Nothing

miterate f = ana (\a-> Just (a, f a))

mmap f = ana aux
    where
        aux (x:xs) = Just (f x, xs)
        aux _ = Nothing

cata ::(a->b->b)->b->[a]->b
cata f v [] = v
cata f v (x:xs) = f x (cata f v xs)


mlength = cata (\a b -> b+1) 0
    where aux _ n = n+1

mfilter f = cata (\a b -> if f a then a:b else b ) []

mmap2 f = cata (\a b -> (f a):b) []

data Expr a b =
    Number b
    | Var a
    | Plus (Expr a b) (Expr a b)

-- (B->C)x(A->C)x(CxC->C)xF -> C
cataexpr :: (b->c)->(a->c)->(c->c->c)->(Expr a b)->c
cataexpr f1 f2 f3 (Number b) = f1 b
cataexpr f1 f2 f3 (Var a)    = f2 a
cataexpr f1 f2 f3 (Plus e1 e2) = f3 (cf e1) (cf e2)
    where
        cf = cataexpr f1 f2 f3

-- (C -> B + A + CxC) x C -> F 
anaexpr :: (c -> Either b (Either a (c,c)))-> c -> Expr a b
anaexpr f c = case (f c) of
    Left b -> Number b
    Right (Left a) -> Var a
    Right (Right (c1, c2)) -> Plus (anaexpr f c1) (anaexpr f c2)

-- Nothing = 0
-- Not really clever but better than undefined/exception
-- 'Eq' is necessary for variable name search
var_get :: (Eq a, Num b) => [(a,b)] -> a -> b
var_get env q =
    case Data.List.lookup q env of
        Just ans -> ans
        Nothing -> 0

eval :: (Eq a, Num b) => [(a,b)] -> Expr a b -> b
eval env = cataexpr (id) (var_get env) ((+))

-- quick manual test:
-- eval [('a',1),('b',10),('c',100)] (Plus (Var 'a')(Plus (Var 'b')(Var 'c')))
-- = 111