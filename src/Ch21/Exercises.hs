module Ch21.Exercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Identity

newtype Identity a =
    Identity a
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
    foldMap f (Identity a) = f a

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

-- Constant

newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
    foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
    traverse _ (Constant a) = pure $ Constant a

instance Eq a => EqProp (Constant a b) where
    (=-=) = eq

instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary

-- Optional

data Optional a
    = Nada
    | Yep a
    deriving (Eq, Show)

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
    foldMap _ Nada = mempty
    foldMap f (Yep a) = f a

instance Traversable Optional where
    traverse _ Nada = pure Nada
    traverse f (Yep a) = Yep <$> f a

instance Eq a => EqProp (Optional a) where
    (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = do
        a <- arbitrary
        elements [Nada, Yep a]

-- List

data List a
    = Nil
    | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
    foldMap _ Nil = mempty
    foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
    traverse _ Nil = pure Nil
    traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance Eq a => EqProp (List a) where
    (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        x <- arbitrary
        xs <- arbitrary
        frequency [ (1, return Nil)
                  , (2, return $ Cons x xs)
                  ]

-- Three

data Three a b c
    = Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
    traverse f (Three a b c) = Three a b <$> f c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary
-- Three'

data Three' a b
    = Three' a b b
    deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance Foldable (Three' a) where
    foldMap f (Three' a b c) = f b <> f c

instance Traversable (Three' a) where
    traverse f (Three' a b c) = Three' a <$> f b <*> f c

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

-- S

data S n a
    = S (n a) a
    deriving (Eq, Show)

instance Functor n => Functor (S n) where
    fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
    foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
    traverse f (S na a) = S <$> traverse f na <*> f a

instance (Eq (n a), Eq a) => EqProp (S n a) where
    (=-=) = eq

instance ( Arbitrary (n a)
         , CoArbitrary (n a)
         , Arbitrary a
         , CoArbitrary a
         ) => Arbitrary (S n a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ S (x y) y

-- Tree

data Tree a
    = Empty
    | Leaf a
    | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Node t1 a t2) = Node (fmap f t1) (f a) (fmap f t2)

instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node t1 a t2) = foldMap f t1 <> f a <> foldMap f t2

instance Traversable Tree where
    traverse _ Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node t1 a t2) = Node <$> traverse f t1
                                     <*> f a
                                     <*> traverse f t2

instance Eq a => EqProp (Tree a) where
    (=-=) = eq

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = do
        a <- arbitrary
        t1 <- arbitrary
        t2 <- arbitrary
        frequency [ (1, return Empty)
                  , (1, return $ Leaf a)
                  , (1, return $ Node t1 a t2)
                  ]

type Trigger = (Int, String, [Int])

spec :: IO ()
spec = do
    let trigger = undefined
    quickBatch $ traversable (trigger :: Identity Trigger)
    quickBatch $ traversable (trigger :: Constant Int Trigger)
    quickBatch $ traversable (trigger :: Optional Trigger)
    quickBatch $ traversable (trigger :: List Trigger)
    quickBatch $ traversable (trigger :: Three Trigger Trigger Trigger)
    quickBatch $ traversable (trigger :: Three' Trigger Trigger)
    quickBatch $ traversable (trigger :: S [] Trigger)
    quickBatch $ traversable (trigger :: Tree Trigger)
