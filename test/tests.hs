{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
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

jinja2 :: Show a => Rendering -> LT.Text -> a -> IO LT.Text
jinja2 rendering template dict = do
  (_code, out, _err) <- readProcessWithExitCode "python" [] script
  return out where
    script = LT.unlines
             [ "from jinja2 import Environment, PackageLoader"
             , "env = Environment(loader=PackageLoader('example', '.'),autoescape=" <> LT.pack (show $ rendering == HTML) <> ")"
             , "template = env.get_template('" <> template <> "')"
             , "print template.render(", LT.pack (show dict), ")"
             ]

case_example :: Assertion
case_example = do
  expected <- jinja2 HTML "example.tmpl" dict
  expected @=? render HTML dict $(haijiFile "example.tmpl") where
    dict = [key|a_variable|] ("Hello,World!" :: T.Text) `merge`
           [key|navigation|] [ [key|caption|] ("A" :: LT.Text) `merge`
                               [key|href|] ("content/a.html" :: String)
                             , [key|caption|] ("&<>'\"\\" :: LT.Text) `merge`
                               [key|href|] ("content/b.html" :: String)
                             ] `merge`
           [key|foo|] (1 :: Int) `merge`
           [key|bar|] ("" :: String)

case_variables :: Assertion
case_variables = do
  expected <- jinja2 HTML "test/variables.tmpl" dict
  expected @=? render HTML dict $(haijiFile "test/variables.tmpl") where
    dict = [key|foo|] ("normal" :: T.Text) `merge`
           [key|_foo|] ("start '_'" :: LT.Text) `merge`
           [key|Foo|] ("start upper case" :: T.Text) `merge`
           [key|F__o_o__|] ("include '_'" :: String) `merge`
           [key|F1a2b3c|] ("include num" :: String)
