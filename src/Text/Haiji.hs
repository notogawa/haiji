{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Text.Haiji
    ( render
    , render'
    , Tmpl
    , haiji
    , haijiFile
    , rawEscape
    , htmlEscape
    , key
    , empty
    , merge
    , unsafeTmpl
    , parseFile
    ) where

import Control.Monad.Trans.Reader
import qualified Data.Aeson as JSON
import Text.Haiji.TH
import Text.Haiji.Types
import Text.Haiji.Dictionary
import Text.Haiji.Unsafe
import Text.Haiji.Parse
import qualified Data.Text.Lazy as LT

render :: Tmpl (Dict s) -> Dict s -> LT.Text
render = runReader

render' :: Tmpl JSON.Value -> JSON.Value -> LT.Text
render' = runReader
