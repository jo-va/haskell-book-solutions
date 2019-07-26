{-# LANGUAGE OverloadedStrings #-}

module Ch27.ByteString where

import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
-- https://hackage.haskell.org/package/zlib
import qualified Codec.Compression.GZip as GZip

input :: BL.ByteString
input = "123"

compressed :: BL.ByteString
compressed = GZip.compress input

testBS :: IO ()
testBS = do
    TIO.putStrLn $ TE.decodeUtf8 (s input)
    TIO.putStrLn $ TE.decodeUtf8 (s compressed)
    where s = BL.toStrict
