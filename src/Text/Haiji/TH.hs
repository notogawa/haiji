{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Text.Haiji.TH where

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
                    , quoteType = haijiType
                    , quoteDec = undefined
                    }

haijiFile :: FilePath -> Q Exp
haijiFile file = runIO (LT.readFile file) >>= haijiExp . LT.unpack

haijiExp :: String -> Q Exp
haijiExp str = case parseOnly parser $ T.pack str of
                 Left err -> error err
                 Right asts -> do
                   esc <- newName "esc"
                   dict <- newName "dict"
                   [e| \ $(varP esc) $(varP dict) -> LT.fromChunks $(listE $ map (haijiAST esc dict) asts) |]


haijiAST esc dict (Literal l) = [e| s |] where s = T.unpack l
haijiAST esc dict (Deref x) = [e| $(varE esc) $ $(deref dict x) |]

deref dict (SimpleVariable v) = [e| retrieve $(varE dict) (Key :: Key $(litT . strTyLit $ T.unpack v)) |]
deref dict (ObjectDotVariable v f) = [e| retrieve $(deref dict v) (Key :: Key $(litT . strTyLit $ T.unpack f)) |]
deref dict (ArrayIndexVariable v ix) = [e| $(deref dict v) !! ix |]

haijiType :: String -> Q Type
haijiType = undefined
