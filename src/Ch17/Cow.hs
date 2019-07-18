module Ch17.Cow where

data Cow = Cow {
    name :: String
  , age :: Int
  , weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name age weight =
    Cow <$> noEmpty name
        <*> noNegative age
        <*> noNegative weight
