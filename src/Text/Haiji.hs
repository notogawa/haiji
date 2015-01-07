{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Text.Haiji
    ( render
    , Rendering(..)
    , Template
    , haiji
    , haijiFile
    ) where

import Text.Haiji.Types
import Text.Haiji.TH
import qualified Data.Text.Lazy as LT

data Rendering = Raw
               | HTML
                 deriving (Eq, Show)

escape :: Rendering -> LT.Text -> LT.Text
escape Raw = id
escape HTML = LT.concatMap replace where
    replace '&'  = "&amp;"
    replace '\\' = "&#92;"
    replace '"'  = "&quot;"
    replace '\'' = "&#39;"
    replace '<'  = "&lt;"
    replace '>'  = "&gt;"
    replace h    = LT.singleton h

type Template require = (LT.Text -> LT.Text) -> TLDict require -> LT.Text

render :: IsTLDict s => Rendering -> TLDict s -> Template s -> LT.Text
render rendering dict template = template (escape rendering) dict
