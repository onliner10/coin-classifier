import GradingSpec qualified as GradingSpec
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Test.Hspec

execTest :: GradingSpec.RawGrades -> SpecWith ()
execTest grades = do
  GradingSpec.tests grades

main :: IO ()
main = do
  currDir <- getCurrentDirectory
  let rawGradesFile = currDir </> "test" </> "raw_grades.txt"
  rawGrades <- GradingSpec.rawGradesFromFile rawGradesFile
  hspec $ execTest rawGrades