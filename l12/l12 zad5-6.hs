import Prelude hiding (fail)

class Monad m => Nondet m where
    amb  :: m a -> m a -> m a
    fail :: m a

data RegExpr =
      Eps -- empty word, epsilon
    | Chr Char -- just a word
    | Alt RegExpr RegExpr -- either this or this
    | Conc RegExpr RegExpr -- concat
    | Kle RegExpr -- Kleene star
    | Plus RegExpr -- Plus = (one or more)
    deriving Show


stringToRe :: String -> RegExpr
stringToRe []   = Eps
stringToRe (a:[]) = Chr a
stringToRe (a:as) = Conc (Chr a) (stringToRe as)

pmatch :: Nondet m => RegExpr -> String -> m String
pmatch (Eps) s = return s

pmatch (Chr c) (cc:cs) = if c == cc then return cs else fail
pmatch (Chr c) _ = fail

pmatch (Alt e1 e2) s = amb (pmatch e1 s) (pmatch e2 s)

pmatch (Conc e1 e2) s = do
    r1 <- pmatch e1 s
    pmatch e2 r1

pmatch star@(Kle e) s = amb (pmatch (Eps) s) (pmatch (Plus e) s)
pmatch plus@(Plus e) s = amb (pmatch e s) (pmatch (Conc e plus) s)


match :: Nondet m => RegExpr -> String -> m ()
match re s = do
    r <- pmatch re s
    if r == "" then
        return ()
    else
        fail


-- monad instances

instance Nondet [] where
    fail = []
    amb = (++)

instance Nondet Maybe where
    fail = Nothing
    amb a b = case a of 
        Just x -> a
        Nothing -> b


listmatch :: RegExpr -> String -> [()]
listmatch = match

maybematch :: RegExpr -> String -> Maybe ()
maybematch = match

-- Maybe breaks the Kleene star (among other things)
-- because it disables retries - if we reach a leaf RE
-- without fail then we never retry matching.
-- The issue exists because `amb` can remember only one calculation
