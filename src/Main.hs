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
import Ch23.RandomExample
import Ch23.RandomExample2
import Ch23.State
import Ch23.FizzBuzz
import Ch23.Exercises
import Ch24.LearnParsers
import Ch24.Fractions
import Ch24.AltParsing
import Ch24.Ini
import Ch24.Marshalling
import Ch24.Semver
import Ch24.Digit
import Ch24.Phone
import Ch24.LogFile
import Ch24.IPV4
import Ch24.IPV6
import Ch25.Identity
import Ch25.Compose
import Ch25.Exercises
import Ch26.MaybeT
import Ch26.EitherT
import Ch26.ReaderT
import Ch26.StateT
import Ch26.OuterInner
import Ch26.Scotty
import Ch26.LoginExercise
import Ch26.Exercises
import Ch26.HitCounter
import Ch27.Kaboom
import Ch27.CoreDump
import Ch27.Trace
import Ch27.ManualBang
import Ch27.Exercises
import Ch27.StrictList
import Ch27.Bench
import Ch27.ListVsMap
import Ch27.MapVsSet
import Ch27.ListVsSequence
import Ch27.ListVsVector
import Ch27.VectorFusion
import Ch27.VectorUpdate
import Ch27.MutableVector
import Ch27.ByteString
import Ch27.Char8ProllyNotWhatYouWant
import Ch27.DList
import Ch27.MVar

main :: IO ()
main = do
    Ch16.Instances.spec
    Ch16.Possibly.spec
    --Ch16.Sum.spec
    --Ch16.Exercises.spec
    Ch17.ZipList.spec
    Ch17.List.spec
    Ch17.Validation.spec
    Ch17.Exercises.spec
    Ch18.Either.spec
    Ch18.BadMonad.spec
    Ch18.Exercises.spec
    Ch21.Exercises.spec
    Ch24.Ini.spec
