module Ch25.Exercises where

class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}

    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second = bimap id

-- Deux

data Deux a b =
    Deux a b
    deriving (Eq, Show)

instance Bifunctor Deux where
    bimap f g (Deux a b) = Deux (f a) (g b)

-- Const

data Const' a b =
    Const' a
    deriving (Eq, Show)

instance Bifunctor Const' where
    bimap f _ (Const' a) = Const' (f a)

-- Drei

data Drei a b c =
    Drei a b c
    deriving (Eq, Show)

instance Bifunctor (Drei a) where
    bimap f g (Drei a b c) = Drei a (f b) (g c)

-- SuperDrei

data SuperDrei a b c =
    SuperDrei a b
    deriving (Eq, Show)

instance Bifunctor (SuperDrei a) where
    bimap f _ (SuperDrei a b) = SuperDrei a (f b)

-- SemiDrei

data SemiDrei a b c =
    SemiDrei a
    deriving (Eq, Show)

instance Bifunctor (SemiDrei a) where
    bimap _ _ (SemiDrei a) = SemiDrei a

-- Quadriceps

data Quadriceps a b c d =
    Quazz a b c d
    deriving (Eq, Show)

instance Bifunctor (Quadriceps a b) where
    bimap f g (Quazz a b c d) = Quazz a b (f c) (g d)

-- Either

data Either' a b =
    Left' a
  | Right' b
  deriving (Eq, Show)

instance Bifunctor Either' where
    bimap f _ (Left' a) = Left' (f a)
    bimap _ g (Right' b) = Right' (g b)
