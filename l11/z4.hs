import Data.Int
import Data.Bits
import Control.Monad
import System.Exit

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


xorit :: [Integer] -> Integer
xorit xs =
    fromIntegral $ foldr (\nx cxor -> xor cxor (fromIntegral nx) ) 
        (0 :: Int32)
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



mastermind :: GState -> GState
mastermind (GState cs) = 
    let cnimsum = xorit cs in
    if cnimsum == 0 then
    GState $ remove1 cs
        else
    case removeN cs cnimsum of
        Nothing -> GState $ remove1 cs
        Just a -> GState a



keke :: GState -> IO ()
keke gsn = 
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

        keke $ mastermind tmp




main :: IO ()
main =
    do
        putStrLn "Welcome to Nim!"
        let cstate = GState $ take (fromIntegral gsize) (iterate (\x->x-1) gsize)
        putStrLn "Your turn!"
        keke cstate