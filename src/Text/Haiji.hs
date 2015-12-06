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
    ) where

import Text.Haiji.TH
import Text.Haiji.Types
import Text.Haiji.Dictionary
