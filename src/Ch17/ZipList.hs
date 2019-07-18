module Ch17.ZipList where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Ch17.List

newtype ZipList' a =
    ZipList' (List a)
    deriving (Eq, Show)

instance Functor ZipList' where
    fmap f (ZipList' l) = ZipList' $ fmap f l

instance Applicative ZipList' where
    pure x = ZipList' (Cons x Nil)
    (<*>) (ZipList' fs) (ZipList' xs) = ZipList' $ zipListWith fs xs

zipListWith :: List (a -> b) -> List a -> List b
zipListWith _ Nil = Nil
zipListWith Nil _ = Nil
zipListWith (Cons f Nil) (Cons x xs) = Cons (f x) (pure f <*> xs)
zipListWith (Cons f fs) (Cons x Nil) = Cons (f x) (fs <*> pure x)
zipListWith (Cons f fs) (Cons x xs) = Cons (f x) (zipListWith fs xs)

instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where
            xs' = let (ZipList' l) = xs
                  in take' 1000 l
            ys' = let (ZipList' l) = ys
                  in take' 1000 l

spec :: IO ()
spec = do
    quickBatch $ applicative (ZipList' (Cons ("a", "b", "c") Nil))
