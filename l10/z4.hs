-- {-#LANGUAGE GADTs #-}

data Colour = Red | Black deriving Show
data RBTree a = RBNode Colour (RBTree a) a (RBTree a) | RBLeaf deriving Show

-- data RBTree a where
--     RBNode :: Ord a => Colour -> (RBTree a) -> a -> (RBTree a) -> (RBTree a)
--     RBLeaf :: RBTree a



rbfix :: RBTree a -> RBTree a
rbfix t = case t of RBNode Black (RBNode Red (RBNode Red a x b) y c) z d -> RBNode Red (RBNode Black a x b) y (RBNode Black c z d)
                    RBNode Black a x (RBNode Red (RBNode Red b y c) z d) -> RBNode Red (RBNode Black a x b) y (RBNode Black c z d)
                    RBNode Black a x (RBNode Red b y (RBNode Red c z d)) -> RBNode Red (RBNode Black a x b) y (RBNode Black c z d)
                    RBNode Black (RBNode Red a x (RBNode Red b y c)) z d -> RBNode Red (RBNode Black a x b) y (RBNode Black c z d)
                    _ -> t

rbnode :: Ord a => Colour -> (RBTree a) -> a -> (RBTree a) -> (RBTree a)
rbnode a b c d =
    rbfix (RBNode a b c d)

rbinsert_aux :: Ord a => a -> RBTree a -> RBTree a
rbinsert_aux x RBLeaf = RBNode Red RBLeaf x RBLeaf
rbinsert_aux x (RBNode cc lt cx rt) =
    if (x <= cx) then
        rbnode cc (rbinsert_aux x lt) cx rt
    else
        rbnode cc lt cx (rbinsert_aux x rt)


-- top-level wrapper
rbinsert :: Ord a => a -> RBTree a -> RBTree a
rbinsert x t =
    RBNode Black a b c
    where
        RBNode _ a b c = rbinsert_aux x t


testing n = foldr (rbinsert) RBLeaf [1..n]