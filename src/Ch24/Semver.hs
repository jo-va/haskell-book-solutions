module Ch24.Semver where

import Control.Applicative
import Text.Trifecta

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Eq, Ord, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
    SemVer Major Minor Patch Release Metadata
    deriving (Eq, Ord, Show)

semverEx = "2.1.1"
semverReleaseEx = "1.0.0-x.7.z.92"
semverReleaseMetaEx = "1.0.0-beta+exp.sha.5114f85"

parseSemVer :: Parser SemVer
parseSemVer = do
    (maj, min, pat) <- parseVersion
    _               <- option '.' (char '-')
    release         <- option [] (some numOrStringDot)
    _               <- option '.' (char '+')
    meta            <- option [] (some numOrStringDot)
    return $ SemVer maj min pat release meta

parseVersion :: Parser (Major, Minor, Patch)
parseVersion = do
    maj <- integer
    _   <- char '.'
    min <- integer
    _   <- char '.'
    pat <- integer
    return (maj, min, pat)

numOrString :: Parser NumberOrString
numOrString = (NOSS <$> some letter)
          <|> (NOSI <$> decimal)

numOrStringDot :: Parser NumberOrString
numOrStringDot = do
    nos <- numOrString
    _   <- skipMany (oneOf ".")
    return nos
