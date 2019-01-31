-- PF 2018 L12

{-# LANGUAGE FunctionalDependencies, FlexibleContexts, FlexibleInstances #-}

import Prelude hiding (LT)
import Control.Monad

-- zad 1 -------------------------------------------------------------------------------------------

import qualified Data.Char as DChr (toLower, isUpper, isDigit, isLetter, isAlphaNum, isSpace, digitToInt)

class Monad m => StreamTrans m i o | m -> i o where
    readS :: m (Maybe i)
    emitS :: o -> m ()

toLower :: StreamTrans m Char Char => m Integer
toLower = aux 0 where
    aux n = do
        cm <- readS
        if cm == Nothing then
            return n
        else
            let (Just c) = cm in
            if DChr.isUpper c then
                do
                    emitS (DChr.toLower c)
                    aux (n+1)
            else
                do
                    emitS c
                    aux n

-- zad 2 ---------------------------------------------------------------------------------------------

instance StreamTrans IO Char Char where
    readS = do
        c <- getChar
        return (Just c)

    emitS = putChar


newtype ListTrans i o a = LT {unLT :: [i] -> ([i], [o], a)}

-- ghc requires all this
instance Functor (ListTrans i o) where
    fmap = liftM

instance Applicative (ListTrans i o) where
    pure = \a -> LT (\is -> (is, [], a))
    (<*>) = ap

instance Monad (ListTrans i o) where
    -- return :: a -> m a
    -- (>>=) :: (ListTrans i o a)->(a -> ListTrans i o b)-> ListTrans i o b
    -- ugliest code in the universe, but it works!
    (>>=) trans con = LT (\xs ->
        let (xs', o, a) = (unLT trans) xs in
            let (xs'', o', b) = (unLT (con a)) xs' in
                    (xs'', o ++ o', b)
        )

instance StreamTrans (ListTrans i o) i o where
    -- readS :: m (Maybe i)
    readS = LT (\x -> 
        case x of [] -> ([], [], Nothing)
                  h:xs -> (xs, [], Just h)
     )
        
    -- emitS :: o -> m ()
    emitS c = LT (\x -> 
            (x, [c], ())
        )
        

toLowerLT :: ListTrans Char Char Integer
toLowerLT = toLower

transform :: ListTrans i o a -> [i] -> ([o], a)
transform lt xs = (os, a) where
    (_, os, a) = (unLT lt) xs


-- zad 3 ---------------------------------------------------------------------------------------------


-- number ::= digit +
-- var ::= letter alphaNum *
-- binop ::= '+' | '-' | '*' | '/'
-- ws ::= blank *
-- expr ::= var | number | expr ws binop ws expr | '(' ws expr ws ')'


data Binop = BOP_PLUS | BOP_MINUS | BOP_MUL | BOP_DIV deriving Show

data Token = TOK_NUM Int
            | TOK_VAR String
            | TOK_BOP Binop
            | TOK_LPAR
            | TOK_RPAR
            | TOK_WS
            | TOK_UNKNOWN Char deriving Show

charToEmitTok :: StreamTrans m Char Token => Char -> m ()
charToEmitTok c =
    case c of
        '(' -> emitS $ TOK_LPAR
        ')' -> emitS $ TOK_RPAR
        '+' -> emitS $ TOK_BOP BOP_PLUS
        '-' -> emitS $ TOK_BOP BOP_MINUS
        '*' -> emitS $ TOK_BOP BOP_MUL
        '/' -> emitS $ TOK_BOP BOP_DIV
        c   -> emitS $ TOK_UNKNOWN c


consumeNum :: StreamTrans m Char Token => Int -> m ()
consumeNum n = do
    c <- readS
    case c of
        Nothing -> do {emitS (TOK_NUM n);}
        Just cc -> if DChr.isSpace cc then
                emitS (TOK_NUM n)
            else
                if DChr.isDigit cc then
                    consumeNum (10*n + (DChr.digitToInt cc))
                else
                    do
                        emitS (TOK_NUM n)
                        charToEmitTok cc
                        lexer



consumeVar :: StreamTrans m Char Token => String -> m ()
consumeVar s = do
    c <- readS
    case c of
        Nothing -> do {emitS (TOK_VAR s);}
        Just cc -> if DChr.isSpace cc then
                emitS (TOK_VAR s)
            else
                if DChr.isAlphaNum cc then
                    consumeVar (s++[cc])
                else
                    do
                        emitS (TOK_VAR s)
                        charToEmitTok cc
                        lexer


isBop :: Char -> Bool
isBop c = elem c "+-*/"

charToBop :: Char -> Binop
charToBop c =
    case c of
        '+' -> BOP_PLUS
        '-' -> BOP_MINUS
        '*' -> BOP_MUL
        '/' -> BOP_DIV

-- I changed the type to ST m Char _Token_ because it's more useful
-- to get a list of tokens than some arbitrary string we'd have
-- to parse again just to get a list of tokens for the parser
-- example usage:
-- > transform lexer "(a+b)/(a*c)"
-- > ([TOK_LPAR,TOK_VAR "a",TOK_BOP BOP_PLUS,TOK_VAR "b",TOK_RPAR,TOK_BOP BOP_DIV,TOK_LPAR,TOK_VAR "a",TOK_BOP BOP_MUL,TOK_VAR "c",TOK_RPAR],())

lexer :: StreamTrans m Char Token => m ()
lexer = do
    c <- readS
    if c == Nothing then 
        return ()
    else
        let (Just cc) = c in
        if DChr.isSpace cc then -- consume whitespace
            lexer
        else
            if DChr.isDigit cc then
                do
                    consumeNum (DChr.digitToInt cc)
                    lexer
            else
            if DChr.isLetter cc then do
                consumeVar [cc]
                lexer
            else
                do
                    charToEmitTok cc
                    lexer



