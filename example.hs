{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Data.Default
import Text.Haiji
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

data Navigation = Navigation {
  caption :: T.Text,
  href :: T.Text
}

data Dict = Dict {
  a_variable :: T.Text,
  navigation :: [Navigation],
  foo :: Int,
  bar :: String
}

main :: IO ()
main = LT.putStr
       $ render $(haijiFile def "example.tmpl")
       $ Dict {
          a_variable = "Hello,World!" :: T.Text,
          navigation = [ Navigation { caption = cap, href = href }
                       | (cap, href) <- [ ("A", "content/a.html")
                                        , ("B", "content/b.html")
                                        ] :: [ (T.Text, T.Text) ]
                       ],
          foo = 1 :: Int,
          bar = "" :: String
       }
