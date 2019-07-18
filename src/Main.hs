module Main where

import Ch16.Instances
import Ch16.Possibly
import Ch16.Sum
import Ch16.Exercises
import Ch17.Lookups
import Ch17.Identity
import Ch17.Constant
import Ch17.MakePerson
import Ch17.Cow
import Ch17.BadMonoid
import Ch17.ZipList
import Ch17.List
import Ch17.Validation
import Ch17.Exercises
import Ch18.FmapBind
import Ch18.Cow
import Ch18.EitherExample
import Ch18.Either
import Ch18.BadMonad
import Ch18.SayHi
import Ch18.Exercises
import Ch18.Exercises2
import Ch20.Library
import Ch20.Exercises
import Ch21.HttpStuff
import Ch21.Either
import Ch21.Tuple
import Ch21.Exercises
import Ch22.NewBeginning
import Ch22.ShortExercise
import Ch22.Person
import Ch22.PrettyReader
import Ch22.ReaderPractice
import Ch22.Shawty

main :: IO ()
main = do
    putStrLn "Chapter 16"

    putStrLn "Functor Instances"
    Ch16.Instances.spec
    Ch16.Possibly.spec
    Ch16.Sum.spec
    Ch16.Exercises.spec

    putStrLn "Chapter 17"
    Ch17.ZipList.spec
    Ch17.List.spec
    Ch17.Validation.spec
    Ch17.Exercises.spec

    putStrLn "Chapter 18"
    Ch18.Either.spec
    Ch18.BadMonad.spec
    Ch18.Exercises.spec

    putStrLn "Chapter 21"
    Ch21.Exercises.spec
