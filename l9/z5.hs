sublist :: [a] -> [[a]]
sublist [] = [[]]
sublist (x:xs) = (sxs) ++ (map (\l->x:l) sxs) where sxs = sublist xs