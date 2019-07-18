module Ch16.Sum where

import Test.QuickCheck
import Test.QuickCheck.Function

import Ch16.FunctorLaws

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

instance (Arbitrary a, Arbitrary b) =>
    Arbitrary (Sum a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [First a, Second b]

type SumCompose =
    Sum Int String
 -> Fun String String
 -> Fun String String
 -> Bool

spec :: IO ()
spec = do
    quickCheck $ \x -> functorIdentity (x :: Sum Int String)
    quickCheck (functorCompose' :: SumCompose)
