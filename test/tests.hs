{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Test.Tasty.TH
import Test.Tasty.HUnit

import Text.Haiji
import Text.Haiji.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import System.Process.Text.Lazy

main :: IO ()
main = $(defaultMainGenerator)

case_example :: Assertion
case_example = do
  (_code, out, _err) <- readProcessWithExitCode "python" []
                        $ LT.unlines [ "from jinja2 import Environment, PackageLoader"
                                     , "env = Environment(loader=PackageLoader('example', '.'))"
                                     , "template = env.get_template('example.tmpl')"
                                     , "print template.render("
                                     , "  {'a_variable' : \"Hello,World!\","
                                     , "  'navigation' : [ { 'href': 'content/a.html', 'caption': 'A'},"
                                     , "                 { 'href': 'content/b.html', 'caption': 'B'} ] }"
                                     , ")"
                                     ]
  out @=? render HTML (asTLDict dict) $(haijiFile "example.tmpl") where
    dict :: TLDict
            '[ "a_variable" :-> T.Text
             , "navigation" :-> [ TLDict
                                  '[ "caption" :-> LT.Text
                                   , "href" :-> String
                                   ]
                                ]
             , "foo" :-> Int
             , "bar" :-> LT.Text
             ]
    dict = Ext (Value "Hello,World!") $
           Ext (Value [ Ext (Value "A") $ Ext (Value "content/a.html") $ Empty
                      , Ext (Value "B") $ Ext (Value "content/b.html") $ Empty
                      ]
               ) $
           Ext (Value 1) $
           Ext (Value "") Empty
