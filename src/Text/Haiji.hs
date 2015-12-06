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
    , unsafeTmpl
    , parseFile
    ) where

import Text.Haiji.TH
import Text.Haiji.Types
import Text.Haiji.Dictionary
import Text.Haiji.Unsafe
import Text.Haiji.Parse
