{-# LANGUAGE NoImplicitPrelude #-}

module Ch22.PrettyReader where

flip :: (a -> b -> c) -> (b -> a -> c)
flip f a b = f b a

const' :: a -> b -> a
const' a b = a

comp :: (b -> c) -> (a -> b) -> (a -> c)
f `comp` g = \a -> f (g a)

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

class Applicative f => Monad f where
    return :: a -> f a
    (>>=) :: f a -> (a -> f b) -> f b

instance Functor ((->) r) where
    fmap = comp

instance Applicative ((->) r) where
    pure = const'
    f <*> a = \r -> f r (a r)

instance Monad ((->) r) where
    return = pure
    m >>= k = flip k <*> m
