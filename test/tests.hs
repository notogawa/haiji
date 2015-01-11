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
import System.Exit
import System.Process.Text.Lazy

main :: IO ()
main = $(defaultMainGenerator)

jinja2 :: Show a => Rendering -> a -> LT.Text -> IO LT.Text
jinja2 rendering dict template = do
  (ExitSuccess, out, _err) <- readProcessWithExitCode "python" [] script
  return out where
    script = LT.unlines
             [ "import sys, codecs, json"
             , "from jinja2 import Environment, PackageLoader"
             , "sys.stdout = codecs.lookup('utf_8')[-1](sys.stdout)"
             , "env = Environment(loader=PackageLoader('example', '.'),autoescape=" <> LT.pack (show $ rendering == HTML) <> ")"
             , "template = env.get_template('" <> template <> "')"
             , "object = json.loads(" <> LT.pack (show $ show dict) <> ")"
             , "print template.render(object)"
             , "exit()"
             ]

case_example :: Assertion
case_example = do
  expected <- jinja2 HTML dict "example.tmpl"
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
  expected <- jinja2 HTML dict "test/variables.tmpl"
  expected @=? render HTML dict $(haijiFile "test/variables.tmpl") where
    dict = [key|foo|] ("normal" :: T.Text) `merge`
           [key|_foo|] ("start '_'" :: LT.Text) `merge`
           [key|Foo|] ("start upper case" :: T.Text) `merge`
           [key|F__o_o__|] ("include '_'" :: String) `merge`
           [key|F1a2b3c|] ("include num" :: String)

case_condition :: Assertion
case_condition = do
  testCondition True  True  True
  testCondition True  True  False
  testCondition True  False True
  testCondition True  False False
  testCondition False True  True
  testCondition False True  False
  testCondition False False True
  testCondition False False False where
    testCondition foo bar baz = do
      expected <- jinja2 HTML dict "test/condition.tmpl"
      expected @=? render HTML dict $(haijiFile "test/condition.tmpl") where
        dict = [key|foo|] foo `merge`
               [key|bar|] bar `merge`
               [key|baz|] baz
