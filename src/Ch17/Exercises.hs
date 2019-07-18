module Ch17.Exercises where

import Control.Applicative (liftA3)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Identity

newtype Identity a =
    Identity a
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure x = Identity x
    (<*>) (Identity f) (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

-- Pair

data Pair a =
    Pair a a
    deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
    pure x = Pair x x
    (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Pair a b

instance Eq a => EqProp (Pair a) where
    (=-=) = eq

-- Two

data Two a b =
    Two a b
    deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure b = Two mempty b
    (<*>) (Two a f) (Two a' x) = Two (a <> a') (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

-- Three

data Three a b c =
    Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure c = Three mempty mempty c
    (<*>) (Three a b f) (Three a' b' x) =
        Three (a <> a') (b <> b') (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

-- Three'

data Three' a b =
    Three' a b b
    deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
    pure b = Three' mempty b b
    (<*>) (Three' a f g) (Three' a' x y) =
        Three' (a <> a') (f x) (g y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three' a b c

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

-- Four

data Four a b c d =
    Four a b c d
    deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) =>
    Applicative (Four a b c) where
    pure d = Four mempty mempty mempty d
    (<*>) (Four a b c f) (Four a' b' c' x) =
        Four (a <> a') (b <> b') (c <> c') (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
    Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq

-- Four'

data Four' a b =
    Four' a a a b
    deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

instance Monoid a => Applicative (Four' a) where
    pure d = Four' mempty mempty mempty d
    (<*>) (Four' a b c f) (Four' a' b' c' x) =
        Four' (a <> a') (b <> b') (c <> c') (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four' a b c d

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq

x = ("a", "a", "a")

stops, vowels :: String
stops = "pbtdkg"
vowels = "aeiou"

combos :: [(Char, Char, Char)]
combos = liftA3 (,,) stops vowels stops

spec :: IO ()
spec = do
    quickBatch $ applicative (Identity x)
    quickBatch $ applicative (Pair x x)
    quickBatch $ applicative (Two x x)
    quickBatch $ applicative (Three x x x)
    quickBatch $ applicative (Three' x x x)
    quickBatch $ applicative (Four x x x x)
    quickBatch $ applicative (Four' x x x x)
