{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Text.Haiji
import Text.Haiji.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

main :: IO ()
main = LT.putStr $ render HTML (asTLDict dict) $(haijiFile "example.tmpl")

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
