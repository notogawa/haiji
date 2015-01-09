{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverlappingInstances #-}

module Main where

import Text.Haiji
import Text.Haiji.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import GHC.TypeLits

class ToPython a where
  toPython  :: a -> LT.Text
  toPython' :: a -> LT.Text
  toPython'= toPython
instance ToPython (TLDict '[]) where
  toPython  Empty = "{}"
  toPython' Empty = " }"
instance (ToPython (TLDict s), ToPython kv) => ToPython (TLDict (kv ': s)) where
  toPython  (Ext x xs) = LT.concat ["{ ", toPython x, toPython' xs]
  toPython' (Ext x xs) = LT.concat [", ", toPython x, toPython' xs]
instance (ToPython v, KnownSymbol k) => ToPython (k :-> v) where
  toPython  x = LT.concat [LT.pack (show $ key x), ": ", toPython (value x)]
instance ToPython v => ToPython [v] where
  toPython  [] = "[]"
  toPython  (x:xs) = LT.concat ["[ ", toPython x, toPython' xs]
  toPython' [] = " ]"
  toPython' (x:xs) = LT.concat [", ", toPython x, toPython' xs]
instance ToPython String where
  toPython  = LT.pack . show
instance ToPython T.Text where
  toPython = LT.pack . show
instance ToPython LT.Text where
  toPython = LT.pack . show
instance ToPython Int where
  toPython = LT.pack . show

main :: IO ()
main = LT.putStr $ render HTML (asTLDict dict) $(haijiFile "example.tmpl")

dict :: TLDict
        '[ "a_variable" :-> T.Text
         , "navigation" :-> [ TLDict
                              '[ "caption" :-> LT.Text
                               , "href" :-> String
                               ]
                            ]
         , "foo" :-> Int
         , "bar" :-> LT.Text
         ]
dict = Ext (Value "Hello,World!") $
       Ext (Value [ Ext (Value "A") $ Ext (Value "content/a.html") $ Empty
                  , Ext (Value "B") $ Ext (Value "content/b.html") $ Empty
                  ]
           ) $
       Ext (Value 1) $
       Ext (Value "") Empty
