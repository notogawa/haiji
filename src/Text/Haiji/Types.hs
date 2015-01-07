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
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.Haiji.Types
    ( TLDict(..)
    , IsTLDict
    , asTLDict
    , merge
    , (:->)(..)
    , Key(..)
    , Retrieve(..) ) where

import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality
import Data.Proxy

data Key (k :: Symbol) where Key :: Key k

infixl 2 :->
data (k :: Symbol) :-> (v :: *) where Value :: v -> k :-> v

newtype VK v k = VK (k :-> v)

value :: k :-> v -> v
value (Value v) = v

key :: KnownSymbol k => k :-> v -> String
key = symbolVal . VK

instance (Show v, KnownSymbol k) => Show (k :-> v) where
    show x = key x ++ " :-> " ++ show (value x)

instance KnownSymbol k => Show (Key k) where
    show = symbolVal

class Retrieve d k v where
    retrieve :: d -> Key k -> v
instance (IsTLDict d, IsTLDict (((k :-> v') ': d)), v' ~ v) => Retrieve (TLDict ((k :-> v') ': d)) k v where
    retrieve (Ext (Value v) _) _ = v
instance (IsTLDict d, IsTLDict (kv ': d), Retrieve (TLDict d) k v) => Retrieve (TLDict (kv ': d)) k v where
    retrieve (Ext _ d) k = retrieve d k

data TLDict (kv :: [*]) where
    Empty :: TLDict '[]
    Ext :: k :-> v -> TLDict d -> TLDict ((k :-> v) ': d)

instance Show (TLDict '[]) where
    show Empty = "{}"
instance (Show kv, Show' (TLDict d)) => Show (TLDict (kv ': d)) where
    show (Ext kv d) = "{" ++ show kv ++ show' d ++ "}"

class Show' d where
    show' :: d -> String
instance Show' (TLDict '[]) where
    show' Empty = ""
instance (Show' (TLDict d), Show kv) => Show' (TLDict (kv ': d)) where
    show' (Ext kv d) = ", " ++ show kv ++ show' d

type AsTLDict s = Normalize (Sort s)

asTLDict :: (Sortable d, Normalizable (Sort d)) => TLDict d -> TLDict (AsTLDict d)
asTLDict = normalize . quicksort

type IsTLDict d = (d ~ Normalize (Sort d))

type Merge xs ys = Normalize (Sort (xs :++ ys))

merge :: (Mergeable a b) => TLDict a -> TLDict b -> TLDict (Merge a b)
merge a b = asTLDict $ append a b

type Mergeable a b = (Sortable (a :++ b), Normalizable (Sort (a :++ b)))

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
    Append '[] ys = ys
    Append (x ': xs) ys = x ': Append xs ys

type (xs :: [k]) :++ (ys :: [k]) = Append xs ys

append :: TLDict xs -> TLDict ys -> TLDict (xs :++ ys)
append Empty ys = ys
append (Ext x xs) ys = Ext x (append xs ys)

type family Normalize d :: [*] where
    Normalize '[]           = '[]
    Normalize '[kv]         = '[kv]
    Normalize ((k :-> v1) ': (k :-> v2) ': d) = Normalize ((k :-> v2) ': d) -- select last one
    Normalize (kv1 ': kv2 ': d) = kv1 ': Normalize (kv2 ': d)

class Normalizable d where
    normalize :: TLDict d -> TLDict (Normalize d)
instance Normalizable '[] where
    normalize d = d
instance Normalizable '[kv] where
    normalize d = d
instance Normalizable ((k :-> v2) ': d) => Normalizable ((k :-> v1) ': (k :-> v2) ': d) where
    normalize (Ext _ d) = normalize d
instance (Normalize (x ': y ': d) ~ (x ': Normalize (y ': d)), Normalizable (y ': d)) => Normalizable (x ': y ': d) where
    normalize (Ext x d) = Ext x (normalize d)

type family Sort (xs :: [k]) :: [k] where
    Sort '[]       = '[]
    Sort (x ': xs) = Sort (Filter FMin x xs) :++ '[x] :++ Sort (Filter FMax x xs)

data Flag = FMin | FMax

type family Cmp (a :: k) (b :: k) :: Ordering
type instance Cmp (k1 :-> v1) (k2 :-> v2) = CmpSymbol k1 k2

type family Filter (f :: Flag) (p :: k) (xs :: [k]) :: [k] where
    Filter f    p '[]       = '[]
    Filter FMin p (x ': xs) = If (Cmp x p == LT) (x ': Filter FMin p xs) (Filter FMin p xs)
    Filter FMax p (x ': xs) = If (Cmp x p == GT || Cmp x p == EQ) (x ': Filter FMax p xs) (Filter FMax p xs)

class Sortable xs where
    quicksort :: TLDict xs -> TLDict (Sort xs)
instance Sortable '[] where
    quicksort Empty = Empty
instance ( Sortable (Filter FMin p xs)
         , Sortable (Filter FMax p xs)
         , FilterV FMin p xs
         , FilterV FMax p xs) => Sortable (p ': xs) where
    quicksort (Ext p xs) = quicksort (less p xs) `append`
                           Ext p Empty           `append`
                           quicksort (more p xs)
        where
          less = filterV (Proxy :: Proxy FMin)
          more = filterV (Proxy :: Proxy FMax)

class FilterV (f::Flag) p xs where
    filterV :: Proxy f -> p -> TLDict xs -> TLDict (Filter f p xs)
instance FilterV f p '[] where
    filterV _ _ _ = Empty

class Conder g where
    cond :: Proxy g -> TLDict s -> TLDict t -> TLDict (If g s t)
instance Conder True where
    cond _ s _ = s
instance Conder False where
    cond _ _ t = t

instance (Conder (Cmp x p == LT), FilterV FMin p xs) => FilterV FMin p (x ': xs) where
    filterV f@Proxy p (Ext x xs) =
        cond
        (Proxy :: Proxy (Cmp x p == LT))
        (Ext x (filterV f p xs))
        (filterV f p xs)
instance (Conder (Cmp x p == GT || Cmp x p == EQ), FilterV FMax p xs) => FilterV FMax p (x ': xs) where
    filterV f@Proxy p (Ext x xs) =
        cond
        (Proxy :: Proxy (Cmp x p == GT || Cmp x p == EQ))
        (Ext x (filterV f p xs))
        (filterV f p xs)
