{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Text.Haiji.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Text.Haiji.Parse
import Text.Haiji.Types
import Data.Convertible

haiji :: QuasiQuoter
haiji = QuasiQuoter { quoteExp = haijiExp
                    , quotePat = undefined
                    , quoteType = haijiType
                    , quoteDec = undefined
                    }

haijiExp :: String -> Q Exp
haijiExp str = case parseOnly parser $ T.pack str of
                 Left err -> error err
                 Right asts -> do
                   esc <- newName "esc"
                   dict <- newName "dict"
                   [e| \ $(varP esc) $(varP dict) -> LT.fromChunks $(listE $ map (haijiAST esc dict) asts) |]

haijiAST esc dict (Literal l) = [e| s |] where s = T.unpack l
haijiAST esc dict (Deref (SimpleVariable v)) = do
  let symbol = litT . strTyLit $ T.unpack v
  [e| $(varE esc) $ retrieve (convert $(varE dict) :: TLDict '[$(symbol) :-> T.Text]) (Key :: Key $(symbol)) |]

haijiType :: String -> Q Type
haijiType = undefined
