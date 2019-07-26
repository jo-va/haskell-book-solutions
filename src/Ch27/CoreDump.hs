module Ch27.CoreDump where

discriminatory :: Bool -> Int
discriminatory b =
    case b of
      False -> 0
      True  -> 1

-- To see the core dump in the REPL:
-- :set -ddump-simpl
-- :set -dsuppress-all
-- :l src/Ch27/CoreDump.hs
