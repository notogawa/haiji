module Main ( main ) where

import Test.DocTest

import System.FilePath
import System.Environment

main :: IO ()
main = do
  confDistDir <- fmap (takeDirectory . takeDirectory . takeDirectory) getExecutablePath
  doctest [ "-isrc"
          , "-itest"
          , "-i" ++ confDistDir ++ "/build/autogen/"
          , "-optP-include"
          , "-optP" ++ confDistDir ++ "/build/autogen/cabal_macros.h"
          , "Text.Haiji"
          ]
