{-# LANGUAGE OverloadedStrings #-}
module Text.Haiji.Rendering
       ( Rendering(..)
       , escape
       ) where

import qualified Data.Text.Lazy as LT

data Rendering = Raw
               | HTML
                 deriving (Eq, Show)

escape :: Rendering -> LT.Text -> LT.Text
escape Raw = id
escape HTML = LT.concatMap replace where
  replace '&'  = "&amp;"
  replace '"'  = "&#34;"
  replace '\'' = "&#39;"
  replace '<'  = "&lt;"
  replace '>'  = "&gt;"
  replace h    = LT.singleton h
