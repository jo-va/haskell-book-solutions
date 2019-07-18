module Ch18.FmapBind where

import Control.Monad

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
       then [x*x, x*x]
       else []

