module Ch24.IPV4 where

import Text.Trifecta
import Data.Word

data IPAddress =
    IPAddress Word32
    deriving (Eq, Ord, Show)

parseIPv4 :: Parser IPAddress
parseIPv4 = do
    n4 <- integerRange 0 255
    _  <- char '.'
    n3 <- integerRange 0 255
    _  <- char '.'
    n2 <- integerRange 0 255
    _  <- char '.'
    n1 <- integerRange 0 255
    return $ IPAddress . fromIntegral $
        n4 * 2^(3*8) + n3 * 2^(2*8) + n2 * 2^8 + n1

integerRange :: Integer -> Integer -> Parser Integer
integerRange min max = do
    n <- integer
    let result
         | n < min   = fail $ "number below " ++ show min
         | n > max   = fail $ "number above " ++ show max
         | otherwise = return n
     in result

spec :: IO ()
spec = do
    print $ parseString parseIPv4 mempty "172.16.254.1"
    print $ parseString parseIPv4 mempty "204.120.0.15"
