import Prelude hiding ((^^))

(^^) :: (b -> c) -> (a -> b) -> a -> c
(^^) f g = f . g

sprintf :: ((a -> a) -> String -> t) -> t
sprintf k = k (id) ""

lit :: String -> (String -> a) -> String -> a
lit s k str = k $ str ++ s

eol :: (String -> a) -> String -> a
eol k   str = k $ str ++ "\n"

int :: (String -> a) -> String -> Integer -> a
int k str n = k $ str ++ show n

flt :: (String -> a) -> String -> Float -> a
flt k str f = k $ str ++ show f

str :: (String -> a) -> String -> String -> a
str k str s = k $ str ++ s

example :: Integer -> String
example n = sprintf (lit "Ala ma " ^^ int ^^ lit " kot" ^^ str ^^ lit ".") n
            (if n == 1 then "a" else if (1 < n && n<5) then "y" else "ow")