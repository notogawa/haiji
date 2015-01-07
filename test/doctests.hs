module Main where

import Test.DocTest

import System.FilePath
import System.Posix.Files

getConfDistDir :: IO FilePath
getConfDistDir = fmap (dirname . dirname . dirname) getModuleFile where
  dirname = takeDirectory
  getModuleFile = readSymbolicLink "/proc/self/exe" -- ghc 7.6.1 以降なら getExecutablePath

main :: IO ()
main = do
  confDistDir <- getConfDistDir
  doctest [ "-isrc"
          , "-itest"
          , "-i" ++ confDistDir ++ "/build/autogen/"
          , "-optP-include"
          , "-optP" ++ confDistDir ++ "/build/autogen/cabal_macros.h"
          , "Text.Haiji"
          ]
