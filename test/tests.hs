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
    dict = $(haijiDict [d| a_variable = "Hello,World!" :: T.Text
                           navigation = [ $(haijiDict [d| caption = "A" :: LT.Text
                                                          href = "content/a.html" :: String
                                                       |])
                                        , $(haijiDict [d| caption = "&<>'\"\\"
                                                          href = "content/b.html"
                                                       |])
                                        ]
                           foo = 1 :: Int
                           bar = "" :: String
                        |])
