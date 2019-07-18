{-# LANGUAGE InstanceSigs #-}

module Ch23.State where

newtype Moi s a =
    Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) = Moi $ \s ->
        let (a, s') = g s
         in (f a, s')

instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi $ \s -> (a, s)

    (<*>) :: Moi s (a -> b)
          -> Moi s a
          -> Moi s b
    Moi sab <*> Moi sa = Moi $ \s ->
        let (a, s') = sa s
            (f, s'') = sab s'
         in (f a, s'')

instance Monad (Moi s) where
    return = pure

    (>>=) :: Moi s a
          -> (a -> Moi s b)
          -> Moi s b
    Moi sa >>= f = Moi $ \s ->
        let (a, s') = sa s
         in runMoi (f a) s'
