{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Text.Haiji.TH ( haiji, haijiFile, key ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Text.Haiji.Parse
import Text.Haiji.Types hiding (key)

haiji :: QuasiQuoter
haiji = QuasiQuoter { quoteExp = haijiExp
                    , quotePat = undefined
                    , quoteType = haijiType
                    , quoteDec = undefined
                    }

haijiFile :: FilePath -> ExpQ
haijiFile file = runIO (LT.readFile file) >>= haijiExp . LT.unpack

haijiExp :: String -> ExpQ
haijiExp = either error haijiASTs . parseOnly parser . T.pack

key :: QuasiQuoter
key = QuasiQuoter { quoteExp = \k -> [e| \v -> singleton v (Key :: Key $(litT . strTyLit $ k)) |]
                , quotePat = undefined
                , quoteType = undefined
                , quoteDec = undefined
                }

haijiASTs :: [AST] -> ExpQ
haijiASTs asts = do
  esc <- newName "esc"
  dict <- newName "dict"
  [e| \ $(varP esc) $(varP dict) -> LT.concat $(listE $ map (haijiAST esc dict) asts) |]

singleton :: x -> Key k -> TLDict '[ k :-> x ]
singleton x _ = Ext (Value x) Empty

haijiAST :: Name -> Name -> AST -> ExpQ
haijiAST _esc _dict (Literal l) =
    [e| s |] where s = T.unpack l
haijiAST  esc  dict (Deref x) =
    [e| $(varE esc) $ toLT $ $(deref dict x) |]
haijiAST  esc  dict (Condition p ts (Just fs)) =
    [e| (if $(deref dict p) then $(haijiASTs ts) else $(haijiASTs fs)) $(varE esc) $(varE dict) |]
haijiAST  esc  dict (Condition p ts Nothing) =
    [e| (if $(deref dict p) then $(haijiASTs ts) else (\_ _ -> "")) $(varE esc) $(varE dict) |]
haijiAST  esc  dict (Foreach k xs body) =
    [e| LT.concat $ map (\x -> $(haijiASTs body) $(varE esc) ($(varE dict) `merge` singleton x (Key :: Key $(litT . strTyLit $ show k)))) $(deref dict xs)|]
haijiAST  esc  dict (Include file) =
    [e| $(haijiFile file) $(varE esc) $(varE dict) |]

class ToLT a where toLT :: a -> LT.Text
instance ToLT String  where toLT = LT.pack
instance ToLT T.Text  where toLT = LT.fromStrict
instance ToLT LT.Text where toLT = id
instance ToLT Int     where toLT = toLT . show
instance ToLT Integer where toLT = toLT . show

deref :: Name -> Variable -> ExpQ
deref dict (Simple v) =
    [e| retrieve $(varE dict) (Key :: Key $(litT . strTyLit $ show v)) |]
deref dict (Attribute v f) =
    [e| retrieve $(deref dict v) (Key :: Key $(litT . strTyLit $ show f)) |]
deref dict (At v ix) =
    [e| $(deref dict v) !! ix |]

haijiType :: String -> Q Type
haijiType = undefined
