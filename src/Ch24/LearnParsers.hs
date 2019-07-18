module Ch24.LearnParsers where

import Control.Monad.Trans.State
import Text.Trifecta
import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

one = char '1'
two = char '2'
three = char '3'

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

testEOF :: Parser () -> IO ()
testEOF p = print $ parseString p mempty "123"

strParser :: String -> Parser String
strParser s = string s

charParser :: [Char] -> Parser [Char]
charParser [] = return []
charParser (x:xs) = do
    char x
    charParser xs
    return (x:xs)

testString :: Parser String -> IO ()
testString p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

testAll = do
    pNL "one eof:"
    testEOF $ one >> eof
    pNL "oneTwo eof:"
    testEOF $ one >> two >> eof
    pNL "one two three eof:"
    testEOF $ one >> two >> three >> eof
    pNL "stop:"
    testParse stop
    pNL "one:"
    testParse one
    pNL "one stop:"
    testParse $ one >> stop
    pNL "one two:"
    testParse $ one >> two
    pNL "one two stop:"
    testParse $ one >> two >> stop
    pNL "string parsers:"
    testString $ strParser "1"
    testString $ strParser "12"
    testString $ strParser "123"
    pNL "string parsers using char:"
    testString $ charParser "1"
    testString $ charParser "12"
    testString $ charParser "123"
