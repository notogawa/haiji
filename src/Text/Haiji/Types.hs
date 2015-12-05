{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Text.Haiji.Types
       ( Tmpl
       , RenderSettings(..)
       , Escape
       , escapeBy
       , rawEscape
       , htmlEscape
       , ToLT
       , toLT
       ) where

import Control.Monad.Trans.Reader
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

data Escape = Escape { unEscape :: LT.Text -> LT.Text }

escapeBy :: LT.Text -> Escape -> LT.Text
escapeBy = flip unEscape

rawEscape :: Escape
rawEscape = Escape id

htmlEscape :: Escape
htmlEscape = Escape (LT.concatMap replace) where
  replace '&'  = "&amp;"
  replace '"'  = "&#34;"
  replace '\'' = "&#39;"
  replace '<'  = "&lt;"
  replace '>'  = "&gt;"
  replace h    = LT.singleton h

data RenderSettings dict =
  RenderSettings { renderSettingsDict :: dict
                 , renderSettingsEscape :: Escape
                 }

type Tmpl dict = Reader (RenderSettings dict) LT.Text

class ToLT a where toLT :: a -> LT.Text
instance ToLT String  where toLT = LT.pack
instance ToLT T.Text  where toLT = LT.fromStrict
instance ToLT LT.Text where toLT = id
instance ToLT Int     where toLT = toLT . show
instance ToLT Integer where toLT = toLT . show
