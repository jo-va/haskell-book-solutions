{-# LANGUAGE OverloadedStrings #-}

module Ch24.Fractions where

import Control.Applicative
import Data.Attoparsec.Text (parseOnly)
import Data.Ratio ((%))
import Data.String (IsString)
import Text.Trifecta

badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

parseFraction :: (Monad m, TokenParsing m) => m Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    case denominator of
      0 -> fail "Denominator cannot be zero"
      _ -> return (numerator % denominator)

testFraction :: IO ()
testFraction = do
    -- parseOnly is attoParsec
    print $ parseOnly parseFraction badFraction
    print $ parseOnly parseFraction shouldWork
    print $ parseOnly parseFraction shouldAlsoWork
    print $ parseOnly parseFraction alsoBad

    -- parseString is trifecta
    print $ parseString parseFraction mempty badFraction
    print $ parseString parseFraction mempty shouldWork
    print $ parseString parseFraction mempty shouldAlsoWork
    print $ parseString parseFraction mempty alsoBad

type NumberOrFraction = Either Double Rational

parseNumberOrFraction :: Parser NumberOrFraction
parseNumberOrFraction =
    (Left <$> try double) <|> (Right <$> try parseFraction)

testNumberOrFraction :: IO ()
testNumberOrFraction = do
    print $ parseString parseNumberOrFraction mempty shouldWork
    print $ parseString parseNumberOrFraction mempty "10.5"

parseInteger :: Parser Integer
parseInteger = do
    x <- integer
    e <- eof
    return x

testInteger :: IO ()
testInteger = do
    print $ parseString parseInteger mempty "123"
    print $ parseString parseInteger mempty "123abc"

