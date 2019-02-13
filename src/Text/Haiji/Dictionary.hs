{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
module Text.Haiji.Dictionary
       ( Dict(..)
       , toDict
       , (:->)
       , empty
       , singleton
       , merge
       , Key(..)
       , retrieve
       ) where

import Data.Aeson
import Data.Dynamic
import qualified Data.HashMap.Strict as M
import Data.Maybe
#if MIN_VERSION_base(4,11,0)
#else
import Data.Monoid
#endif
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Type.Bool
import Data.Type.Equality
#if MIN_VERSION_base(4,9,0)
import Data.Kind
#define STAR Type
#else
#define STAR *
#endif
import GHC.TypeLits

data Key (k :: Symbol) where
  Key :: KnownSymbol k => Key k

infixl 2 :->
data (k :: Symbol) :-> (v :: STAR)

-- | Empty dictionary
empty :: Dict '[]
empty = Dict M.empty

singleton :: Typeable x => x -> Key k -> Dict '[ k :-> x ]
singleton x k = Dict $ M.singleton (keyVal k) (toDyn x)

-- | Create single element dictionary (with TypeApplications extention)
toDict :: (KnownSymbol k, Typeable x) => x -> Dict '[ k :-> x ]
toDict = flip singleton Key

keyVal :: Key k -> String
keyVal k = case k of Key -> symbolVal k

retrieve :: Typeable (Retrieve xs k) => Dict xs -> Key k -> Retrieve xs k
retrieve (Dict d) k = fromJust $ fromDynamic $ d M.! keyVal k

type family Retrieve (a :: [STAR]) (b :: Symbol) where
  Retrieve ((kx :-> vx) ': xs) key = If (CmpSymbol kx key == 'EQ) vx (Retrieve xs key)

-- | Type level Dictionary
data Dict (kv :: [STAR]) = Dict (M.HashMap String Dynamic)

instance ToJSON (Dict '[]) where
  toJSON _ = object []

instance (ToJSON (Dict d), ToJSON v, KnownSymbol k, Typeable v) => ToJSON (Dict ((k :-> v) ': d)) where
  toJSON dict = Object (a <> b) where
    (x, v, xs) = headKV dict
    Object a = object [ T.pack (keyVal x) .= v ]
    Object b = toJSON xs
    headKV :: (KnownSymbol k, Typeable v) => Dict ((k :-> v) ': d) -> (Key k, v, Dict d)
    headKV (Dict d) = (k, fromJust $ fromDynamic $ d M.! keyVal k, Dict $ M.delete (keyVal k) d) where
      k = Key

instance ToJSON (Dict s) => Show (Dict s) where
  show = LT.unpack . LT.decodeUtf8 . encode

merge :: Dict xs -> Dict ys -> Dict (Merge xs ys)
merge (Dict x) (Dict y) = Dict (y `M.union` x)

type family Merge a b :: [STAR] where
  Merge xs '[] = xs
  Merge '[] ys = ys
  Merge (x ': xs) (y ': ys) = If (Cmp x y == 'EQ) (y ': Merge xs ys) (If (Cmp x y == 'LT) (x ': Merge xs (y ': ys)) (y ': Merge (x ': xs) ys))

type family Cmp (a :: k) (b :: k) :: Ordering where
  Cmp (k1 :-> v1) (k2 :-> v2) = CmpSymbol k1 k2
