module Ch20.Exercises where

-- Constant

data Constant a b =
    Constant a
    deriving (Eq, Show)

instance Foldable (Constant a) where
    foldMap _ _ = mempty

-- Two

data Two a b =
    Two a b
    deriving (Eq, Show)

instance Foldable (Two a) where
    foldMap f (Two _ b) = f b

-- Three

data Three a b c =
    Three a b c
    deriving (Eq, Show)

instance Foldable (Three a b) where
    foldMap f (Three _ _ c) = f c

-- Three'

data Three' a b =
    Three' a b b
    deriving (Eq, Show)

instance Foldable (Three' a) where
    foldMap f (Three' _ b c) = (f b) <> (f c)

-- Four'

data Four' a b =
    Four' a b b b
    deriving (Eq, Show)

instance Foldable (Four' a) where
    foldMap f (Four' _ b c d) = (f b) <> (f c) <> (f d)

-- filterF

filterF
    :: (Applicative f, Foldable f, Monoid (f a))
    => (a -> Bool)
    -> f a
    -> f a
filterF f = foldMap go
  where
    go x = if f x then pure x else mempty
