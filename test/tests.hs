{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Test.Tasty.TH
import Test.Tasty.HUnit

import Control.Monad
import Text.Haiji
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.Exit
import System.Process.Text.Lazy

main :: IO ()
main = $(defaultMainGenerator)

jinja2 :: Show a => Rendering -> a -> LT.Text -> IO LT.Text
jinja2 rendering dict template = do
  (code, out, err) <- readProcessWithExitCode "python" [] script
  unless (code == ExitSuccess) $ LT.putStrLn err
  return out where
    script = LT.unlines
             [ "import sys, codecs, json"
             , "from jinja2 import Environment, PackageLoader"
             , "sys.stdout = codecs.lookup('utf_8')[-1](sys.stdout)"
             , "env = Environment(loader=PackageLoader('example', '.'),autoescape=" <> LT.pack (show $ rendering == HTML) <> ")"
             , "template = env.get_template('" <> template <> "')"
             , "object = json.loads(" <> LT.pack (show $ show dict) <> ")"
             , "print template.render(object),"
             , "exit()"
             ]

case_example :: Assertion
case_example = do
  expected <- jinja2 HTML dict "example.tmpl"
  expected @=? render HTML dict $(haijiFile "example.tmpl") where
    dict = [key|a_variable|] ("Hello,World!" :: T.Text) `merge`
           [key|navigation|] [ [key|caption|] ("A" :: LT.Text) `merge`
                               [key|href|] ("content/a.html" :: String)
                             , [key|caption|] ("B" :: LT.Text) `merge`
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

case_HTML_escape :: Assertion
case_HTML_escape = do
  expected <- jinja2 HTML dict "test/HTML_escape.tmpl"
  expected @=? render HTML dict $(haijiFile "test/HTML_escape.tmpl") where
    dict = [key|foo|] ([' '..'\126'] :: String)

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

case_foreach :: Assertion
case_foreach = do
  expected <- jinja2 HTML dict "test/foreach.tmpl"
  expected @=? render HTML dict $(haijiFile "test/foreach.tmpl") where
    dict = [key|foo|] ([0,2..10] :: [Int])

case_foreach_shadowing :: Assertion
case_foreach_shadowing = do
  expected <- jinja2 HTML dict "test/foreach.tmpl"
  expected @=? render HTML dict $(haijiFile "test/foreach.tmpl")
  False @=? ("bar" `LT.isInfixOf` expected) where
    dict = [key|foo|] ([0,2..10] :: [Int]) `merge`
           [key|bar|] ("bar" :: String)

case_include :: Assertion
case_include = do
  testInclude ([0..10] :: [Int])
  testInclude (["","\n","\n\n"] :: [String]) where
    testInclude xs = do
      expected <- jinja2 HTML dict "test/include.tmpl"
      expected @=? render HTML dict $(haijiFile "test/include.tmpl") where
        dict = [key|foo|] xs

case_raw :: Assertion
case_raw = do
  expected <- jinja2 HTML dict "test/raw.tmpl"
  expected @=? render HTML dict $(haijiFile "test/raw.tmpl") where
    dict = [key|foo|] ([0,2..10] :: [Int]) `merge`
           [key|bar|] ("bar" :: String)
