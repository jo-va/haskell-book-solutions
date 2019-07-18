module Ch17.Identity where

newtype Ident a =
    Ident a
    deriving (Eq, Ord, Show)

instance Functor Ident where
    fmap f (Ident a) = Ident (f a)

instance Applicative Ident where
    pure = Ident
    (<*>) (Ident f)
          (Ident a) = Ident (f a)
