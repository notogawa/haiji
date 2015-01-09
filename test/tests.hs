{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Test.Tasty.TH
import Test.Tasty.HUnit

import Data.Aeson
import Text.Haiji
import Text.Haiji.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import System.Process.Text.Lazy

import Test.Util ()

main :: IO ()
main = $(defaultMainGenerator)

case_example :: Assertion
case_example = do
  (_code, out, _err) <- readProcessWithExitCode "python" []
                        $ LT.unlines [ "from jinja2 import Environment, PackageLoader"
                                     , "env = Environment(loader=PackageLoader('example', '.'))"
                                     , "template = env.get_template('example.tmpl')"
                                     , "print template.render(", LT.decodeUtf8 (encode dict), ")"
                                     ]
  out @=? render HTML (asTLDict dict) $(haijiFile "example.tmpl") where
    dict = Ext (Value "Hello,World!" :: "a_variable" :-> T.Text) $
           Ext (Value [ Ext (Value "A") $ Ext (Value "content/a.html") $ Empty
                      , Ext (Value "B") $ Ext (Value "content/b.html") $ Empty
                      ] :: "navigation" :-> [ TLDict
                                              '[ "caption" :-> LT.Text
                                               , "href" :-> String
                                               ]
                                            ]
               ) $
           Ext (Value 1 :: "foo" :-> Int) $
           Ext (Value "" :: "bar" :-> LT.Text) Empty
