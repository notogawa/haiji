{-# LANGUAGE CPP #-}
module Text.Haiji.Utils where

import qualified Data.Aeson as JSON

#if MIN_VERSION_aeson(2,0,0)
import Data.String (fromString)
import qualified Data.Aeson.KeyMap as JSON

toKey :: String -> JSON.Key
toKey = fromString

insertValue :: JSON.Key -> JSON.Value -> JSON.KeyMap JSON.Value -> JSON.KeyMap JSON.Value
insertValue = JSON.insert

#else
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

toKey :: String -> T.Text
toKey = T.pack

insertValue :: T.Text -> JSON.Value -> HM.HashMap T.Text JSON.Value -> HM.HashMap T.Text JSON.Value
insertValue = HM.insert
#endif



