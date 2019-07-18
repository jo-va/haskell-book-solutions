{-# LANGUAGE FlexibleInstances #-}

module Ch16.Exercises where

import Test.QuickCheck
import Test.QuickCheck.Function

import Ch16.FunctorLaws

-- Sum

data Sum a b =
    First b
  | Second a
  deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (First a) = First (f a)
    fmap _ (Second b) = Second b

instance (Arbitrary a, Arbitrary b) =>
    Arbitrary (Sum a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [First a, Second b]

type SumIdentity = Sum Int String
type SumCompose =
    SumIdentity
 -> Fun String String
 -> Fun String String
 -> Bool

-- Company

data Company a b c =
    DeepBlue a b
  | Something c
  deriving (Eq, Show)

instance Functor (Company a b) where
    fmap _ (DeepBlue a b) = DeepBlue a b
    fmap f (Something c) = Something (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Company a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        elements [DeepBlue a b, Something c]

type CompanyIdentity = Company String String Int
type CompanyCompose =
    CompanyIdentity 
 -> Fun Int Int
 -> Fun Int Int
 -> Bool

-- More

data More a b =
    L b a b
  | R a b a
  deriving (Eq, Show)

instance Functor (More a) where
    fmap f (L b a b') = L (f b) a (f b')
    fmap f (R a b a') = R a (f b) a'

instance (Arbitrary a, Arbitrary b) => Arbitrary (More a b) where
    arbitrary = do
        a <- arbitrary
        a' <- arbitrary
        b <- arbitrary
        b' <- arbitrary
        elements [L b a b', R a b a']

type MoreIdentity = More Int String
type MoreCompose =
    MoreIdentity
 -> Fun String String
 -> Fun String String
 -> Bool

-- Quant

data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Finance, Desk a, Bloor b]

type QuantIdentity = Quant Int String
type QuantCompose =
    QuantIdentity
 -> Fun String String
 -> Fun String String
 -> Bool

-- K

data K a b =
    K a
    deriving (Eq, Show)

instance Functor (K a) where
    fmap f (K a) = K a

instance Arbitrary a => Arbitrary (K a b) where
    arbitrary = do
        a <- arbitrary
        return $ K a

type KIdentity = K String Int
type KCompose =
    KIdentity
 -> Fun Int Int
 -> Fun Int Int
 -> Bool

-- Flip

newtype Flip f a b =
    Flip (f b a)
    deriving (Eq, Show)

instance Functor (Flip K a) where
    fmap f (Flip (K x)) = Flip $ K (f x) 

-- EvilGoateeConst

data EvilGoateeConst a b =
    GoatyConst b
    deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

instance Arbitrary b => Arbitrary (EvilGoateeConst a b) where
    arbitrary = do
        b <- arbitrary
        return $ GoatyConst b

type EvilGoateeConstIdentity = EvilGoateeConst Int String
type EvilGoateeConstCompose =
    EvilGoateeConstIdentity
 -> Fun String String
 -> Fun String String
 -> Bool

-- LiftItOut

data LiftItOut f a =
    LiftItOut (f a)
    deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-- Parappa

data Parappa f g a =
    DaWrappa (f a) (g a)
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- IgnoreOne

data IgnoreOne f g a b =
    IgnoringSomething (f a) (g b)
    deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

-- Notorious

data Notorious g o a t =
    Notorious (g o) (g a) (g t)
    deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- List

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- GoatLord

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

-- TalkToMe

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read g) = Read (f . g)

spec :: IO ()
spec = do
    quickCheck $ \x -> functorIdentity (x :: SumIdentity)
    quickCheck (functorCompose' :: SumCompose)
    quickCheck $ \x -> functorIdentity (x :: CompanyIdentity)
    quickCheck (functorCompose' :: CompanyCompose)
    quickCheck $ \x -> functorIdentity (x :: MoreIdentity)
    quickCheck (functorCompose' :: MoreCompose)
    quickCheck $ \x -> functorIdentity (x :: QuantIdentity)
    quickCheck (functorCompose' :: QuantCompose)
    quickCheck $ \x -> functorIdentity (x :: KIdentity)
    quickCheck (functorCompose' :: KCompose)
    quickCheck $ \x -> functorIdentity (x :: EvilGoateeConstIdentity)
    quickCheck (functorCompose' :: EvilGoateeConstCompose)
