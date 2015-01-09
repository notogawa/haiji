{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Util where

import Data.Aeson
import Data.Monoid
import Data.Text
import GHC.TypeLits
import Text.Haiji.Types

instance ToJSON (TLDict '[]) where
  toJSON Empty = object []

instance (ToJSON (TLDict s), ToJSON kv) => ToJSON (TLDict (kv ': s)) where
  toJSON (Ext x xs) = Object (a <> b) where
    Object a = toJSON x
    Object b = toJSON xs

instance (ToJSON v, KnownSymbol k) => ToJSON (k :-> v) where
  toJSON x = object [ pack (key x) .= value x ]
