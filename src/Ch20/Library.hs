module Ch20.Library where

import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (\y -> Any (y == x))

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr go Nothing
  where
    go x Nothing  = Just x
    go x (Just y) = Just (min x y)

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr go Nothing
  where
    go x Nothing  = Just x
    go x (Just y) = Just (max x y)

null' :: Foldable t => t a -> Bool
null' = foldr go True
  where
    go _ _ = False

length' :: Foldable t => t a -> Int
length' = foldr go 0
  where
    go _ n = n + 1

toList' :: Foldable t => t a -> [a]
toList' = foldMap (:[])

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr go mempty
  where
    go x acc = f x <> acc
