{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Text.Haiji
import Text.Haiji.Types
import Text.Haiji.TH
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

main :: IO ()
main = LT.putStr $ render HTML dict $(haijiFile "example.tmpl") where
    dict :: TLDict
            '[ "a_variable" :-> T.Text
             , "navigation" :-> [ TLDict
                                  '[ "href" :-> String
                                   , "caption" :-> LT.Text
                                   ]
                                ]
             , "foo" :-> Int
             , "bar" :-> LT.Text
             ]
    dict = Ext (Value "Hello,World!") $
           Ext (Value [ Ext (Value "content/a.html") $ Ext (Value "A") Empty
                      , Ext (Value "content/b.html") $ Ext (Value "B") Empty
                      ]
               ) $
           Ext (Value 1) $
           Ext (Value "") Empty
