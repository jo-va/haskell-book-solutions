module Ch27.Bench where

import Criterion.Main

infixl 9 !?
_      !? n | n < 0 = Nothing
[]     !? _         = Nothing
(x:_)  !? 0         = Just x
(_:xs) !? n         = xs !? (n-1)

infixl 9 !#
{-# INLINABLE (!#) #-}
(!#) :: [a] -> Int -> Maybe a
xs !# n
  | n < 0     = Nothing
  | otherwise =
        foldr (\x r k -> case k of
            0 -> Just x
            _ -> r (k-1)) (const Nothing) xs n

myList :: [Int]
myList = [1..9999]

listBench :: IO ()
listBench = defaultMain
    [ bench "map list 9999" $ nf (map (+1)) myList ]

mainBench :: IO ()
mainBench = defaultMain
    [ bench "!! 9999"
      $ whnf (myList !!) 9998
    , bench "!? 9999"
      $ whnf (myList !?) 9998
    , bench "!# 9999"
      $ whnf (myList !#) 9998
    ]
