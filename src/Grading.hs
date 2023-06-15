module Grading (grade) where

import Control.Monad.Except (MonadError (throwError))
import Model (RawGrading)

data ScaleGrade = State1 | State2

data NCGSGrade = Gr1 | Gr2

data Grade = Scale ScaleGrade | NCGS NCGSGrade

grade :: (MonadError String m) => RawGrading -> m Grade
grade _ = throwError "not implemented"
