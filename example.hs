{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Text.Haiji
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

main :: IO ()
main = LT.putStr $ render HTML dict $(haijiFile "example.tmpl") where
  dict = [key|a_variable|] ("Hello,World!" :: T.Text) `merge`
         [key|navigation|] [ [key|caption|] ("A" :: LT.Text) `merge`
                             [key|href|] ("content/a.html" :: String)
                           , [key|caption|] ("B" :: LT.Text) `merge`
                             [key|href|] ("content/b.html" :: String)
                           ] `merge`
         [key|foo|] (1 :: Int) `merge`
         [key|bar|] ("" :: String)
