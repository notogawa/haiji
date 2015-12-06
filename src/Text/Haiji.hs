{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Text.Haiji
    ( render
    , Tmpl
    , Environments
    , Dict
    , haiji
    , haijiFile
    , empty
    , key
    , merge
    , unsafeTmpl
    , parseFile
    ) where

import Text.Haiji.TH
import Text.Haiji.Types
import Text.Haiji.Dictionary
import Text.Haiji.Unsafe
import Text.Haiji.Parse
