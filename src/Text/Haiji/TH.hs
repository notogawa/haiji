{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Haiji.TH ( haiji, haijiFile, key ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Text.Haiji.Parse
import Text.Haiji.Types

haiji :: QuasiQuoter
haiji = QuasiQuoter { quoteExp = haijiExp
                    , quotePat = undefined
                    , quoteType = undefined
                    , quoteDec = undefined
                    }

haijiFile :: FilePath -> ExpQ
haijiFile file = runIO (LT.readFile file) >>= haijiExp . LT.unpack . deleteLastOneLF where
  deleteLastOneLF xs
    | "%}\n" `LT.isSuffixOf` xs     = LT.init xs
    | "\n\n" `LT.isSuffixOf` xs     = LT.init xs
    | not ("\n" `LT.isSuffixOf` xs) = xs `LT.append` "\n"
    | otherwise                     = xs

haijiImportFile :: FilePath -> ExpQ
haijiImportFile file = runIO (LT.readFile file) >>= haijiExp . LT.unpack . deleteLastOneLF where
  deleteLastOneLF xs
    | LT.null xs         = xs
    | LT.last xs == '\n' = LT.init xs
    | otherwise          = xs

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

haijiAST :: Name -> Name -> AST -> ExpQ
haijiAST esc dict (Literal l) =
    [e| (\_ _ -> s) $(varE esc) $(varE dict) |] where s = T.unpack l
haijiAST esc dict (Deref x) =
    [e| $(varE esc) $ toLT $ $(deref dict x) |]
haijiAST esc dict (Condition p ts (Just fs)) =
    [e| (if $(deref dict p) then $(haijiASTs ts) else $(haijiASTs fs)) $(varE esc) $(varE dict) |]
haijiAST esc dict (Condition p ts Nothing) =
    [e| (if $(deref dict p) then $(haijiASTs ts) else (\_ _ -> "")) $(varE esc) $(varE dict) |]
haijiAST esc dict (Foreach k xs loopBody elseBody) =
    [e| let dicts = $(deref dict xs)
            len = length dicts
        in if 0 < len
           then LT.concat
                $ map (\(ix, x) -> $(haijiASTs loopBody)
                                   $(varE esc)
                                   ($(varE dict) `merge`
                                    singleton x (Key :: Key $(litT . strTyLit $ show k)) `merge`
                                    singleton (loopVariables len ix) (Key :: Key "loop")))
                $ zip [0..] dicts
           else $(maybe [e| (\_ _ -> "") |] haijiASTs elseBody) $(varE esc) $(varE dict)
      |]
haijiAST esc dict (Include file) =
    [e| $(haijiImportFile file) $(varE esc) $(varE dict) |]
haijiAST esc dict (Raw raw) =
    [e| (\_ _ -> raw) $(varE esc) $(varE dict) |]

loopVariables :: Int -> Int -> TLDict '["first" :-> Bool, "index" :-> Int, "index0" :-> Int, "last" :-> Bool, "length" :-> Int, "revindex" :-> Int, "revindex0" :-> Int]
loopVariables len ix =
  Ext (Value (ix == 0)       :: "first"     :-> Bool) $
  Ext (Value (ix + 1)        :: "index"     :-> Int ) $
  Ext (Value ix              :: "index0"    :-> Int ) $
  Ext (Value (ix == len - 1) :: "last"      :-> Bool) $
  Ext (Value len             :: "length"    :-> Int ) $
  Ext (Value (len - ix)      :: "revindex"  :-> Int ) $
  Ext (Value (len - ix - 1)  :: "revindex0" :-> Int ) $
  Empty

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
