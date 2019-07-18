module Ch17.Validation where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a =
    Error' e
  | Success' a
  deriving (Eq, Show)

instance Functor (Validation e) where
    fmap _ (Error' e) = Error' e
    fmap f (Success' a) = Success' (f a)

instance Monoid e => Applicative (Validation e) where
    pure x = Success' x
    (<*>) (Error' e) (Error' e') = Error' (e <> e')
    (<*>) (Error' e) _ = Error' e
    (<*>) _ (Error' e) = Error' e
    (<*>) (Success' f) (Success' a) = Success' (f a)

instance (Arbitrary e, Arbitrary a) =>
    Arbitrary (Validation e a) where
    arbitrary = do
        e <- arbitrary
        a <- arbitrary
        elements [Error' e, Success' a]

instance (Eq e, Eq a) => EqProp (Validation e a) where
    (=-=) = eq

trigger :: Validation String (String, String, String)
trigger = undefined

spec :: IO ()
spec = quickBatch $ applicative trigger
