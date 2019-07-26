module Ch27.Exercises where

x = undefined
y = x `seq` "blah"

testBottom = do
    print (snd (x, y))
