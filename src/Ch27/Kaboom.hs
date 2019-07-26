module Ch27.Kaboom where

possiblyKaboom =
    \f -> f fst snd (0, undefined)

true :: a -> a -> a
true = \a -> (\b -> a)

false :: a -> a -> a
false = \a -> (\b -> b)
