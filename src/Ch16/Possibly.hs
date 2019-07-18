module Ch16.Possibly where

import Test.QuickCheck
import Test.QuickCheck.Function

import Ch16.FunctorLaws

-- Possibly

data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
    fmap _ LolNope = LolNope
    fmap f (Yeppers a) = Yeppers (f a)

instance Arbitrary a =>
    Arbitrary (Possibly a) where
    arbitrary = do
        a <- arbitrary
        elements [LolNope, Yeppers a]

type PossiblyCompose =
    Possibly String
 -> Fun String String
 -> Fun String String
 -> Bool

spec :: IO ()
spec = do
    quickCheck $ \x -> functorIdentity (x :: Possibly String)
    quickCheck (functorCompose' :: PossiblyCompose)
