{-# LANGUAGE ViewPatterns #-}

module Ch16.FunctorLaws where

import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
       (a -> b)
    -> (b -> c)
    -> f a
    -> Bool
functorCompose f g x =
    (fmap (g . f) x) == (fmap g . fmap f $ x)

functorCompose' :: (Eq (f c), Functor f) =>
       f a
    -> Fun a b
    -> Fun b c
    -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
    (fmap (g . f) x) == (fmap g . fmap f $ x)
