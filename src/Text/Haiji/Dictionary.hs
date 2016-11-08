{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Text.Haiji.Dictionary
       ( Dict(..)
       , toDict
       , (:->)(..)
       , empty
       , singleton
       , merge
       , Key(..)
       , retrieve
       ) where

import Data.Aeson
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Type.Bool
import GHC.TypeLits
import Data.Singletons
import Data.Singletons.Prelude

data Key (k :: Symbol) where
  Key :: KnownSymbol k => Key k

infixl 2 :->
data (k :: Symbol) :-> (v :: *) where KV :: Key k -> v -> k :-> v

-- | Empty dictionary
empty :: Dict '[]
empty = Empty

singleton :: x -> Key k -> Dict '[ k :-> x ]
singleton x k = Ext (KV k x) Empty

-- | Create single element dictionary (with TypeApplications extention)
toDict :: KnownSymbol k => x -> Dict '[ k :-> x ]
toDict = flip singleton Key

value :: k :-> v -> v
value (KV _ v) = v

key :: k :-> v -> Key k
key (KV k _)= k

keyVal :: Key k -> String
keyVal k = case k of Key -> symbolVal k

retrieve :: Dict xs -> Key k -> Retrieve xs k
retrieve (Ext (KV k v) dicts) k' = case (k, k') of
  (Key, Key) -> case singByProxy k %:== singByProxy k' of
    SFalse -> retrieve dicts k'
    STrue  -> v

type family Retrieve (a :: [*]) (b :: Symbol) where
  Retrieve ((kx :-> vx) ': xs) key = If (kx :== key) vx (Retrieve xs key)

-- | Type level Dictionary
data Dict (kv :: [*]) where
  Empty :: Dict '[]
  Ext :: k :-> v -> Dict d -> Dict ((k :-> v) ': d)

instance ToJSON (Dict '[]) where
  toJSON Empty = object []

instance (ToJSON (Dict s), ToJSON kv) => ToJSON (Dict (kv ': s)) where
  toJSON (Ext x xs) = Object (a <> b) where
    Object a = toJSON x
    Object b = toJSON xs

instance ToJSON v => ToJSON (k :-> v) where
  toJSON x = object [ T.pack (keyVal $ key x) .= value x ]

instance ToJSON (Dict s) => Show (Dict s) where
  show = LT.unpack . LT.decodeUtf8 . encode

merge :: Dict xs -> Dict ys -> Dict (Merge xs ys)
merge xs Empty = xs
merge Empty ys = ys
merge (Ext x xs) (Ext y ys) = case (key x, key y) of
  (Key, Key) -> case singByProxy (key x) %:== singByProxy (key y) of
    STrue  -> Ext y (merge xs ys)
    SFalse -> case singByProxy (key x) %:< singByProxy (key y) of
      STrue -> Ext x (merge xs (Ext y ys))
      SFalse -> Ext y (merge (Ext x xs) ys)

type family Merge a b :: [*] where
  Merge xs '[] = xs
  Merge '[] ys = ys
  Merge ((kx :-> vx) ': xs) ((ky :-> vy) ': ys) = If (kx :== ky) ((ky :-> vy) ': Merge xs ys) (If (kx :< ky) ((kx :-> vx) ': Merge xs ((ky :-> vy) ': ys)) ((ky :-> vy) ': Merge ((kx :-> vx) ': xs) ys))
