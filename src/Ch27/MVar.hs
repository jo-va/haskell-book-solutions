module Ch27.MVar where

import Control.Concurrent

myData :: IO (MVar Int)
myData = newEmptyMVar

testMVar :: IO ()
testMVar = do
    mv <- myData
    putMVar mv (0 :: Int)
    zero <- takeMVar mv
    print zero
