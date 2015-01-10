{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Test.Tasty.TH
import Test.Tasty.HUnit

import Text.Haiji
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import System.Process.Text.Lazy

main :: IO ()
main = $(defaultMainGenerator)

renderByJinja2 :: Show a => LT.Text -> a -> IO LT.Text
renderByJinja2 template dict = do
  (_code, out, _err) <- readProcessWithExitCode "python" [] script
  return out where
    script = LT.unlines
             [ "from jinja2 import Environment, PackageLoader"
             , "env = Environment(loader=PackageLoader('example', '.'))"
             , "template = env.get_template('" <> template <> "')"
             , "print template.render(", LT.pack (show dict), ")"
             ]

case_example :: Assertion
case_example = do
  expected <- renderByJinja2 "example.tmpl" dict
  expected @=? render HTML dict $(haijiFile "example.tmpl") where
    dict = [key|a_variable|] ("Hello,World!" :: T.Text) `merge`
           [key|navigation|] [ [key|caption|] ("A" :: LT.Text) `merge`
                               [key|href|] ("content/a.html" :: String)
                             , [key|caption|] ("B" :: LT.Text) `merge`
                               [key|href|] ("content/b.html" :: String)
                             ] `merge`
           [key|foo|] (1 :: Int) `merge`
           [key|bar|] ("" :: String)
