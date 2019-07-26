module Ch24.Phone where

import Control.Monad
import Control.Applicative
import Text.Trifecta

type NumberingPlanArea = Integer -- aka area code
type Exchange = Integer
type LineNumber = Integer

data PhoneNumber =
    PhoneNumber NumberingPlanArea Exchange LineNumber
    deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
    _        <- option "" (string "1-")
    plan     <- try bracketPlan <|> parse3Digits
    _        <- option '-' separators
    exchange <- parse3Digits
    _        <- option '-' separators
    line     <- parse4Digits
    _        <- eof
    return $ PhoneNumber plan exchange line

bracketPlan :: Parser Integer
bracketPlan = char '(' *> parse3Digits <* char ')'

parse3Digits :: Parser Integer
parse3Digits = digitRange 3 100 999

parse4Digits :: Parser Integer
parse4Digits = digitRange 4 1000 9999

separators :: Parser Char
separators = oneOf [' ', '-']

digitRange :: Int -> Integer -> Integer -> Parser Integer
digitRange n min max = do
    xs <- replicateM n digit
    let n = read xs
        result
          | n < min   = fail $ "number below " ++ show min
          | n > max   = fail $ "number above " ++ show max
          | otherwise = return n
     in result

spec :: IO ()
spec = do
    print $ parseString parsePhone mempty "123-456-7890"
    print $ parseString parsePhone mempty "1234567890"
    print $ parseString parsePhone mempty "(123) 456-7890"
    print $ parseString parsePhone mempty "1-123-456-7890"
    print $ parseString parsePhone mempty "(123)4567890"
    print $ parseString parsePhone mempty "(123) 456 7890"
    print $ parseString parsePhone mempty "(123)-456-7890"
    print $ parseString parsePhone mempty "1-(123)-4567890"
    print $ parseString parsePhone mempty "1-(123)-45678901"
