{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Text.Haiji where

import Text.Haiji.Types
import Data.Convertible
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

sampleDict :: TLDict '["foo" :-> T.Text, "bar" :-> Int]
sampleDict = Ext (Value "<script></script>") $ Ext (Value 2) $ Empty

sampleTmpl :: Template '[ "foo" :-> T.Text ]
sampleTmpl esc x = TL.fromChunks ["<h1>", esc $ retrieve x (Key :: Key "foo"), "</h1>" ]

render :: Convertible (TLDict super) (TLDict sub) => Rendering -> TLDict super -> Template sub -> TL.Text
render rendering dict template = template esc (convert dict) where
    esc = escape rendering
