module Ch22.ShortExercise where

import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled1 :: [Char] -> ([Char], [Char])
tupled1 = (,) <$> cap <*> rev

tupled2 :: [Char] -> ([Char], [Char])
tupled2 = do
    a <- cap
    b <- rev
    return (a, b)

tupled3 :: [Char] -> ([Char], [Char])
tupled3 =
    cap >>= \a ->
    rev >>= \b ->
    return (a, b)
