module Ch24.Digit where

import Control.Applicative
import Text.Trifecta

parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9'] <?> "parseDigit error"

base10Pos :: Parser Integer
base10Pos = do
    d <- some parseDigit
    return $ read d

base10Pos' :: Parser Integer
base10Pos' = some parseDigit >>= (return . read)

base10Neg :: Parser Integer
base10Neg = do
    _ <- char '-'
    n <- base10Pos
    return $ negate n

base10Both :: Parser Integer
base10Both = try base10Pos <|> try base10Neg

spec :: IO ()
spec = do
    print $ parseString parseDigit mempty "123"
    print $ parseString parseDigit mempty "abc"
    print $ parseString base10Pos mempty "123"
    print $ parseString base10Pos mempty "abc"
    print $ parseString base10Pos mempty "123abc"
    print $ parseString base10Neg mempty "-123"
    print $ parseString base10Neg mempty "-123abc"
    print $ parseString base10Both mempty "-123abc"
