{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
#if MIN_VERSION_base(4,8,0)
#else
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits

data Key (k :: Symbol) where Key :: Key k

infixl 2 :->
data (k :: Symbol) :-> (v :: *) where Value :: v -> k :-> v

newtype VK v k = VK (k :-> v)

-- | Empty dictionary
empty :: Dict '[]
empty = Empty

singleton :: x -> Key k -> Dict '[ k :-> x ]
singleton x _ = Ext (Value x) Empty

toDict :: forall k x . x -> Dict '[ k :-> x ]
toDict = flip singleton Key

value :: k :-> v -> v
value (Value v) = v

key :: KnownSymbol k => k :-> v -> String
key = symbolVal . VK

class Retrieve d k v where
  retrieve :: d -> Key k -> v
#if MIN_VERSION_base(4,8,0)
instance {-# OVERLAPPABLE #-} Retrieve (Dict d) k v => Retrieve (Dict (kv ': d)) k v where
#else
instance                      Retrieve (Dict d) k v => Retrieve (Dict (kv ': d)) k v where
#endif
  retrieve (Ext _ d) k = retrieve d k
#if MIN_VERSION_base(4,8,0)
instance {-# OVERLAPPING #-} v' ~ v => Retrieve (Dict ((k :-> v') ': d)) k v where
#else
instance                     v' ~ v => Retrieve (Dict ((k :-> v') ': d)) k v where
#endif
  retrieve (Ext (Value v) _) _ = v

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

instance (ToJSON v, KnownSymbol k) => ToJSON (k :-> v) where
  toJSON x = object [ T.pack (key x) .= value x ]

instance ToJSON (Dict s) => Show (Dict s) where
  show = LT.unpack . LT.decodeUtf8 . encode

class Mergeable xs ys where
  merge :: Dict xs -> Dict ys -> Dict (Merge xs ys)
instance Mergeable xs '[] where
  merge xs Empty = xs
instance Mergeable '[] (y ': ys) where
  merge Empty ys = ys
instance (Conder (Cmp x y == 'EQ), Conder (Cmp x y == 'LT), Mergeable xs ys, Mergeable (x ': xs) ys, Mergeable xs (y ': ys)) => Mergeable (x ': xs) (y ': ys) where
  merge (Ext x xs) (Ext y ys) = cond (Proxy :: Proxy (Cmp x y == 'EQ))
                                     (Ext y (merge xs ys))
                                     (cond (Proxy :: Proxy (Cmp x y == 'LT))
                                           (Ext x (merge xs (Ext y ys)))
                                           (Ext y (merge (Ext x xs) ys)))

type family Merge a b :: [*] where
  Merge xs '[] = xs
  Merge '[] ys = ys
  Merge (x ': xs) (y ': ys) = If (Cmp x y == 'EQ)
                                  (y ': Merge xs ys) -- select last one
                                  (If (Cmp x y == 'LT)
                                      (x ': Merge xs (y ': ys))
                                      (y ': Merge (x ': xs) ys))

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

type family Cmp (a :: k) (b :: k) :: Ordering
type instance Cmp (k1 :-> v1) (k2 :-> v2) = CmpSymbol k1 k2

class Conder g where
  cond :: Proxy g -> Dict s -> Dict t -> Dict (If g s t)
instance Conder 'True where
  cond _ s _ = s
instance Conder 'False where
  cond _ _ t = t
