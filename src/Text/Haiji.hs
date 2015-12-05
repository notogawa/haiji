{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Text.Haiji
    ( render
    , Tmpl
    , haiji
    , haijiFile
    , rawEscape
    , htmlEscape
    , key
    , empty
    , merge
    ) where

import Control.Monad.Trans.Reader
import qualified Data.Aeson as JSON
import Text.Haiji.TH
import Text.Haiji.Types
import Text.Haiji.Dictionary
import qualified Data.Text.Lazy as LT


render :: Escape -> Dict s -> Tmpl (Dict s) -> LT.Text
render escape dict template = runReader template $ RenderSettings dict escape

render' :: Escape -> Dict s -> Tmpl JSON.Value -> LT.Text
render' escape dict template = undefined
