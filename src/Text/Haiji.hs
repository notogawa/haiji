{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Text.Haiji
    ( render
    , Rendering(..)
    , Template
    ) where

import Text.Haiji.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

data Rendering = Raw
               | HTML
                 deriving (Eq, Show)

escape :: Rendering -> T.Text -> T.Text
escape Raw = id
escape HTML = T.concatMap replace where
    replace '&'  = "&amp;"
    replace '\\' = "&#92;"
    replace '"'  = "&quot;"
    replace '\'' = "&#39;"
    replace '<'  = "&lt;"
    replace '>'  = "&gt;"
    replace h    = T.singleton h

type Template require = (T.Text -> T.Text) -> TLDict require -> TL.Text

render :: Rendering -> TLDict s -> Template s -> TL.Text
render rendering dict template = template (escape rendering) dict
