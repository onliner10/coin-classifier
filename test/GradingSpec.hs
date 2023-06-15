module GradingSpec (tests, rawGradesFromFile, RawGrades) where

import Control.Monad (forM_)
import Data.Bifunctor (Bifunctor (first))
import Data.String.Interpolate (i)
import Data.Text (Text, lines)
import Data.Text.IO (readFile)
import Grading (grade)
import Model (RawGrading (RawGrading))
import Test.Hspec

newtype RawGrades = RawGrades {unRawGrades :: [Text]}

rawGradesFromFile :: FilePath -> IO RawGrades
rawGradesFromFile path = RawGrades . Data.Text.lines <$> Data.Text.IO.readFile path

tests :: RawGrades -> SpecWith ()
tests (RawGrades examples) = describe "Grading.grade" $ do
  it "grades all examples" $ do
    let rawGrades = fmap RawGrading examples
        res :: Either Text ()
        res = forM_ rawGrades (\input -> first (\err -> [i|#{err}: '#{input}'|]) $ grade input)
    res `shouldBe` Right ()