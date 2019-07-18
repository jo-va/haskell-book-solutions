module Ch16.Instances where

import Test.QuickCheck
import Test.QuickCheck.Function

import Ch16.FunctorLaws

-- Identity

newtype Identity a =
    Identity a
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Arbitrary a =>
    Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary 
        return $ Identity a

-- Pair

data Pair a =
    Pair a a
    deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

instance Arbitrary a =>
    Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Pair a b

-- Two

data Two a b =
    Two a b
    deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) =>
    Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

-- Three

data Three a b c =
    Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

-- Three'

data Three' a b =
    Three' a b b
    deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) =>
    Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three' a b c

-- Four

data Four a b c d =
    Four a b c d
    deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
    Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d

-- Four'

data Four' a b =
    Four' a a a b
    deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) =>
    Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four' a b c d


type IntToInt = Fun Int Int
type StringToString = Fun String String

type IdentityCompose =
    Identity Int -> IntToInt -> IntToInt -> Bool

type PairCompose =
    Pair String -> StringToString -> StringToString -> Bool

type TwoCompose =
    Two String Int -> IntToInt -> IntToInt -> Bool

type ThreeCompose =
    Three String Int String -> StringToString -> StringToString -> Bool

type Three'Compose =
    Three' String Int -> IntToInt -> IntToInt -> Bool

type FourCompose =
    Four String Int String Int -> IntToInt -> IntToInt -> Bool

type Four'Compose =
    Four' String Int -> IntToInt -> IntToInt -> Bool

spec :: IO ()
spec = do
    quickCheck $ \x -> functorIdentity (x :: Identity [Int])
    quickCheck (functorCompose' :: IdentityCompose)

    quickCheck $ \x -> functorIdentity (x :: Pair String)
    quickCheck (functorCompose' :: PairCompose)

    quickCheck $ \x -> functorIdentity (x :: Two String Int)
    quickCheck (functorCompose' :: TwoCompose)

    quickCheck $ \x -> functorIdentity (x :: Three String Int String)
    quickCheck (functorCompose' :: ThreeCompose)

    quickCheck $ \x -> functorIdentity (x :: Three' String Int)
    quickCheck (functorCompose' :: Three'Compose)

    quickCheck $ \x -> functorIdentity (x :: Four String Int String Int)
    quickCheck (functorCompose' :: FourCompose)

    quickCheck $ \x -> functorIdentity (x :: Four' String Int)
    quickCheck (functorCompose' :: Four'Compose)

