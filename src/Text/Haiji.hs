{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Text.Haiji
    ( render
    , Rendering(..)
    , Template
    , haiji
    , haijiFile
    , key
    , merge
    ) where

import Text.Haiji.TH
import Text.Haiji.Types
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

type Template require = (LT.Text -> LT.Text) -> TLDict require -> LT.Text

render :: Rendering -> TLDict s -> Template s -> LT.Text
render rendering dict template = template (escape rendering) dict
