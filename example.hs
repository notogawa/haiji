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
  dict = $(haijiDict [d| a_variable = "Hello,World!" :: T.Text
                         navigation = [ $(haijiDict [d| caption = "A" :: LT.Text
                                                        href = "content/a.html" :: String
                                                     |])
                                      , $(haijiDict [d| caption = "B"
                                                        href = "content/b.html"
                                                     |])
                                      ]
                         foo = 1 :: Int
                         bar = "" :: String
                      |])
