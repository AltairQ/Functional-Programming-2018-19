import Control.Monad
import Data.Int
import Data.Bits
import Control.Monad
import System.Exit

class Monad m => Random m where
    random :: m Int

newtype RS t = RS {unRS :: Int -> (Int, t)}

-- ghc requires all this
instance Functor (RS) where
    fmap = liftM

instance Applicative (RS) where
    pure = \a -> RS (\i -> (i, a))
    (<*>) = ap

instance Monad (RS) where
    -- >>= :: (m a) -> (a -> m b) -> m b
    (>>=) trans con = RS (
        \n -> let (nn, a) = (unRS trans) n in
            (unRS (con a)) (a2b nn)
        )

a2b :: Int -> Int
a2b a = 
    if b <= 0 then
        b + 2147483647
    else
        b
    where
        b = 16807*(a `mod` 127773) - 2836 * (a `div` 127773)

toPair :: a -> (a, a)
toPair x = (x, x)

instance Random RS where
    random = RS (toPair . a2b)



-- the game starts here ************************************************************


-- game size
gsize = 5 :: Integer

newtype GState = GState [Integer]

numToStars :: Integer -> String
numToStars n = take (fromIntegral n) (repeat '*')


pprint :: [Integer] -> Integer -> String
pprint [] _ = ""
pprint (x:xs) k =
    show k ++ ": " ++ numToStars x ++ "\n" ++ pprint xs (k+1)

instance Show GState where
    show (GState gs) =
        pprint gs 1



readInt :: IO Integer
readInt =
    do
        s <- getLine
        return $ read s -- this is weird


applyMoveRaw :: [Integer] -> Integer -> Integer -> [Integer]
applyMoveRaw (x:xs) 0 amnt =
    if x < amnt
    then
        0 : xs
    else
        (x-amnt):xs

applyMoveRaw (x:xs) k amnt=
    x : (applyMoveRaw xs (k-1) amnt)

applyMove :: GState -> (Integer, Integer) -> GState
applyMove (GState gs) (row, amnt) =
    GState (applyMoveRaw gs (row-1) amnt)


maxit :: [Integer] -> Integer
maxit xs =
    fromIntegral $ foldr (\a b -> max a b ) 
        (0)
        xs


isEnded :: GState -> Bool
isEnded (GState []) = True
isEnded (GState (x:xs)) =
    (x == 0) && (isEnded $ GState xs)



removeN :: [Integer] -> Integer -> Maybe [Integer]
removeN [] _ = Nothing
removeN (x:xs) n =
    if x >= n
    then
        Just ((x-n):xs)
    else
        case (removeN xs n) of
            Just res -> Just $ x : res
            Nothing -> Nothing

remove1 :: [Integer] -> [Integer]
remove1 [] = []
remove1 (x:xs) =
    if x >= 1
    then
        ((x-1):xs)
    else
        x : (remove1 xs)



randomPlay :: GState -> RS GState
randomPlay (GState cs)  = do
    randres <- random
    let maxelem = maxit cs in
        let mychoice = 1+((toInteger randres) `mod` maxelem) in
            case removeN cs mychoice of
                Nothing -> return (GState $ remove1 cs)
                Just a -> return (GState a)


-- It would probably be best to combine the RS and IO monads
-- but I don't know monad transformers yet
gameloop :: GState -> Int -> IO ()
gameloop gsn seed = 
    do
        putStrLn $ show gsn

        when (isEnded gsn) (putStrLn "I won! git gud")
        when (isEnded gsn) (exitSuccess) -- I don't know how to exit...

        putStrLn "Enter row:"
        u_row <- readInt
        putStrLn "Enter amount:"
        u_amt <- readInt
        let tmp = applyMove gsn (u_row, u_amt)

        when (isEnded tmp) (putStrLn "GZ, you won!")
        when (isEnded tmp) (exitSuccess)

        let (nextseed, nextstate) = (unRS (randomPlay tmp)) seed in
            gameloop nextstate nextseed


main :: IO ()
main =
    do
        putStrLn "Welcome to Nim!"
        let cstate = GState $ take (fromIntegral gsize) (iterate (\x->x-1) gsize)
        putStrLn "Your turn!"
        gameloop cstate 42