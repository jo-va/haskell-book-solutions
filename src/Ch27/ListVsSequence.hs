module Ch27.ListVsSequence where

import Criterion.Main
import qualified Data.Sequence as S

lists :: [[Int]]
lists = replicate 10 [1..100000]

seqs :: [S.Seq Int]
seqs = replicate 10 (S.fromList [1..100000])

testListVsSequence :: IO ()
testListVsSequence = defaultMain
    [ bench "concatenate lists" $
      nf mconcat lists
    , bench "concatenate sequences" $
      nf mconcat seqs
    ]

lists' :: [Int]
lists' = [1..100000]

seqs' :: S.Seq Int
seqs' = S.fromList [1..100000]

testListVsSequence' :: IO ()
testListVsSequence' = defaultMain
    [ bench "indexing list" $
      whnf (\xs -> xs !! 9001) lists'
    , bench "indexing sequence" $
      whnf (flip S.index 9001) seqs'
    ]
