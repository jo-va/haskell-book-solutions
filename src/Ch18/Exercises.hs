module Ch18.Exercises where

import Control.Monad (ap, liftM)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Nope

data Nope a =
    NopeDotJpg
    deriving (Eq, Show)

instance Functor Nope where
    fmap = liftM

instance Applicative Nope where
    pure = return
    (<*>) = ap

instance Monad Nope where
    return _ = NopeDotJpg
    _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
    (=-=) = eq

-- PhhhbbtttEither 

data PhhhbbtttEither b a =
    Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
    fmap = liftM

instance Applicative (PhhhbbtttEither b) where
    pure = return
    (<*>) = ap

instance Monad (PhhhbbtttEither b) where
    return = Left'
    Right' b >>= _ = Right' b
    Left' a >>= f = f a

instance (Arbitrary b, Arbitrary a) =>
    Arbitrary (PhhhbbtttEither b a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Left' a, Right' b]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
    (=-=) = eq

-- Identity

newtype Identity a =
    Identity a
    deriving (Eq, Show, Ord)

instance Functor Identity where
    fmap = liftM

instance Applicative Identity where
    pure = return
    (<*>) = ap

instance Monad Identity where
    return = Identity
    Identity a >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

-- List

data List a = 
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
    Nil <> ys = ys
    (Cons x xs) <> ys = Cons x $ xs `mappend` ys

instance Monoid (List a) where
    mempty = Nil

instance Functor List where
    fmap = liftM

instance Applicative List where
    pure = return
    (<*>) = ap

instance Monad List where
    return x = Cons x Nil
    Nil >>= _ = Nil
    Cons a Nil >>= f = f a
    Cons a l >>= f = (f a) <> (l >>= f)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        a <- arbitrary
        l <- arbitrary
        frequency [ (1, return Nil)
                  , (2, return $ Cons a l)
                  ]

instance Eq a => EqProp (List a) where
    (=-=) = eq

spec :: IO ()
spec = do
    let nopeTrigger = undefined :: Nope (Int, String, Int)
    quickBatch $ functor nopeTrigger
    quickBatch $ applicative nopeTrigger
    quickBatch $ monad nopeTrigger

    let eitherTrigger = undefined :: PhhhbbtttEither Int (Int, String, Int)
    quickBatch $ functor eitherTrigger
    quickBatch $ applicative eitherTrigger
    quickBatch $ monad eitherTrigger

    let identityTrigger = undefined :: Identity (Int, String, Int)
    quickBatch $ functor identityTrigger
    quickBatch $ applicative identityTrigger
    quickBatch $ monad identityTrigger

    let listTrigger = undefined :: List (Int, String, Int)
    quickBatch $ functor listTrigger
    quickBatch $ applicative listTrigger
    quickBatch $ monad listTrigger

