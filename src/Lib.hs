module Lib
  ( someFunc,
  )
where

import Model (foo)

someFunc :: IO ()
someFunc = putStrLn foo
