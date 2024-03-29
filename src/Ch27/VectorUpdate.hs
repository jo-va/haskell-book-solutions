module Ch27.VectorUpdate where

import Criterion.Main
import Data.Vector ((//))
import qualified Data.Vector as V

vec :: V.Vector Int
vec = V.fromList [1..10000]

slow :: Int -> V.Vector Int
slow n = go n vec
    where go 0 v = v
          go n v = go (n-1) (v // [(n, 0)])

batchList :: Int -> V.Vector Int
batchList n = vec // updates
    where updates = fmap (\n -> (n, 0)) [0..n]

testVectorUpdate :: IO ()
testVectorUpdate = defaultMain
    [ bench "slow" $ whnf slow 9998
    , bench "batch list" $ whnf batchList 9998
    ]
