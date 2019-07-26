module Ch24.IPV6 where

import Control.Applicative
import Text.Trifecta
import Data.Char (toLower)
import Data.List (elemIndex)

data IP6Address =
    IP6Address Integer
    deriving (Eq, Ord, Show)

data IP6Segment =
    IP6Segment Integer
  | Hole
  deriving (Eq, Show)

ipv6Segments :: Parser [IP6Segment]
ipv6Segments = do
    firstSeg <- hexR 0 65535
    segments <- many ipv6SegmentOrHole
    return $ IP6Segment firstSeg : segments 

ipv6SegmentOrHole :: Parser IP6Segment
ipv6SegmentOrHole = do
    char ':'
    option Hole (IP6Segment <$> (try $ hexR 0 65535))

parseIPv6 :: Parser IP6Address
parseIPv6 = do
    parts <- ipv6Segments
    let n = length parts
        missingZeros = replicate (9 - n) 0
        segments = foldr go [] parts
        go Hole rest = concat [missingZeros, rest]
        go (IP6Segment segment) rest = segment : rest
     in return $ IP6Address . sum $
        zipWith (*) ((65536^) <$> [0..7]) (reverse segments)

hexR :: Integer -> Integer -> Parser Integer
hexR min max = do
    n <- some hexDigit
    let value = readHex n
     in case value of
          Nothing            -> fail $ "invalid hex digit"
          Just v | v < min   -> fail $ "number below min"
          Just v | v > max   -> fail $ "number above max"
          Just v | otherwise -> return v

readHex :: String -> Maybe Integer
readHex s = go (reverse s)
    where go [] = Just 0
          go (x:xs) = (+) <$> hexChar x <*> ((16*) <$> go xs)

hexChar :: Char -> Maybe Integer
hexChar c =
    let validHex = "0123456789abcdef"
     in toInteger <$> elemIndex (toLower c) validHex

spec :: IO ()
spec = do
    -- 281473568538113
    print $ parseString parseIPv6 mempty
        "0:0:0:0:0:ffff:ac10:fe01"
    -- 281473568538113
    print $ parseString parseIPv6 mempty
        "0::ffff:ac10:fe01"
    -- 281474112159759
    print $ parseString parseIPv6 mempty
        "0:0:0:0:0:ffff:cc78:f"
    -- 281474112159759
    print $ parseString parseIPv6 mempty
        "0::ffff:cc78:f"
    -- 338288524927261089654163772891438416681
    print $ parseString parseIPv6 mempty
        "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
    -- 338288524927261089654163772891438416681
    print $ parseString parseIPv6 mempty
        "FE80::0202:B3FF:FE1E:8329"
    -- 42540766411282592856906245548098208122
    print $ parseString parseIPv6 mempty
        "2001:DB8::8:800:200C:417A"
    -- Error
    -- this should fail but it replaces the bad
    -- segment with a Hole...
    print $ parseString parseIPv6 mempty
        "0::fgff:ac10:fe01"
