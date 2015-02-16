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

import Control.Monad.Trans.Reader
import Text.Haiji.TH
import Text.Haiji.Types
import Text.Haiji.Rendering
import qualified Data.Text.Lazy as LT


type Template require = Reader (HaijiParams (TLDict require)) LT.Text

render :: Rendering -> TLDict s -> Template s -> LT.Text
render rendering dict template = runReader template $ HaijiParams dict (escape rendering)
