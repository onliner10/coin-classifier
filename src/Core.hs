module Core (NormalizedWords, toNormalizedWords, containsAll, containsNone) where

import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Text (Text, replace, toUpper, words)

newtype NormalizedWords = NormalizedWords {getWords :: HashSet Text}

toNormalizedWords :: Text -> NormalizedWords
toNormalizedWords = NormalizedWords . HS.fromList . Data.Text.words . replace "." " " . replace "," " " . toUpper

containsAll :: NormalizedWords -> NormalizedWords -> Bool
containsAll (NormalizedWords a) (NormalizedWords b) = (a `HS.intersection` b) == b

containsNone :: NormalizedWords -> NormalizedWords -> Bool
containsNone (NormalizedWords a) (NormalizedWords b) = (a `HS.intersection` b) == mempty