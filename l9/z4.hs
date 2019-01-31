import qualified Data.List as List

insev :: a -> [a] -> [a] -> [[a]]
insev x prev [] = [prev++[x]]
insev x prev (a:as) = 
    (prev++(x:a:as)) : (insev x (prev++[a]) as)

iperm :: [a] -> [[a]]
iperm [] = [[]]
iperm (x:xs) = 
     concat $ map (insev x []) pxs 
     where
         pxs = iperm xs        

sperm :: Eq a => [a] -> [[a]]
sperm [] = [[]]
-- sperm x:[] = [[x]]
sperm xs = foldl (\a x -> a ++ map (\p->x:p) (sperm (List.delete x xs)) ) [] xs
