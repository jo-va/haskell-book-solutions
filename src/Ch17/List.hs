module Ch17.List where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Nil
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Cons f fs) xs = append (fmap f xs) (fs <*> xs)

genList :: Arbitrary a => Gen (List a)
genList = do
    x <- arbitrary
    xs <- genList
    frequency [ (1, return Nil)
              , (2, return (Cons x xs))
              ]

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = genList

instance Eq a => EqProp (List a) where
    xs =-= ys = xs' `eq` ys'
        where
            xs' = take' 1000 xs
            ys' = take' 1000 ys

spec :: IO ()
spec = do
    quickBatch $ applicative (Cons ("a", "b", "c") Nil)
