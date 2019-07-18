module Ch21.Tuple where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Tuple a b =
    Tuple a b
    deriving (Eq, Show)

instance Functor (Tuple a) where
    fmap f (Tuple x y) = Tuple x $ f y

instance Monoid a => Applicative (Tuple a) where
    pure x = Tuple mempty x
    Tuple u f <*> Tuple v x = Tuple (u <> v) (f x)

instance Foldable (Tuple a) where
    foldMap f (Tuple _ y) = f y
    foldr f z (Tuple _ y) = f y z

instance Traversable (Tuple a) where
    traverse f (Tuple x y) = Tuple x <$> f y

instance (Eq a, Eq b) => EqProp (Tuple a b) where
    (=-=) = eq

instance (Arbitrary a, Arbitrary b) =>
    Arbitrary (Tuple a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Tuple a b

trigger :: Tuple (Sum Int) (Int, String, [Int])
trigger = undefined

spec :: IO ()
spec = do
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ traversable trigger

x :: Tuple String Int
x = Tuple "1" 2

f :: a -> Either e a
f = Right

t :: a -> [a]
t x = [x]
