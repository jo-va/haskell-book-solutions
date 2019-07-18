module Ch22.NewBeginning where

import Control.Applicative

hurr :: Num a => a -> a
hurr = (*2)

durr :: Num a => a -> a
durr = (+10)

m :: Num a => a -> a
m = hurr . durr

m' :: Num a => a -> a
m' = fmap hurr durr

m2 :: Num a => a -> a
m2 = (+) <$> hurr <*> durr

m3 :: Num a => a -> a
m3 = liftA2 (+) hurr durr

m4 :: Num a => a -> a
m4 = do
    a <- hurr
    b <- durr
    return (a + b)
