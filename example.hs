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

main :: IO ()
main = LT.putStr
       $ render $(haijiFile def "example.tmpl")
       $ [key|a_variable|] ("Hello,World!" :: T.Text) `merge`
         [key|navigation|] [ [key|caption|] cap `merge` [key|href|] href
                           | (cap, href) <- [ ("A", "content/a.html")
                                            , ("B", "content/b.html")
                                            ] :: [ (T.Text, T.Text) ]
                           ] `merge`
         [key|foo|] (1 :: Int) `merge`
         [key|bar|] ("" :: String)
