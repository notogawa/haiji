{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Haiji.TH ( haiji, haijiFile, key, HaijiParams(..) ) where

import Control.Applicative
import Control.Monad.Trans.Reader
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
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

haijiFile :: Quasi q => FilePath -> q Exp
haijiFile file = runQ (runIO $ LT.readFile file) >>= haijiExp . LT.unpack . deleteLastOneLF where
  deleteLastOneLF xs
    | "%}\n" `LT.isSuffixOf` xs     = LT.init xs
    | "\n\n" `LT.isSuffixOf` xs     = LT.init xs
    | not ("\n" `LT.isSuffixOf` xs) = xs `LT.append` "\n"
    | otherwise                     = xs

haijiImportFile :: Quasi q => FilePath -> q Exp
haijiImportFile file = runQ (runIO $ LT.readFile file) >>= haijiExp . LT.unpack . deleteLastOneLF where
  deleteLastOneLF xs
    | LT.null xs         = xs
    | LT.last xs == '\n' = LT.init xs
    | otherwise          = xs

haijiExp :: Quasi q => String -> q Exp
haijiExp = either error haijiASTs . parseOnly parser . T.pack

key :: QuasiQuoter
key = QuasiQuoter { quoteExp = \k -> [e| \v -> singleton v (Key :: Key $(litT . strTyLit $ k)) |]
                  , quotePat = undefined
                  , quoteType = undefined
                  , quoteDec = undefined
                  }

haijiASTs :: Quasi q => [AST] -> q Exp
haijiASTs asts = runQ [e| LT.concat <$> sequence $(listE $ map haijiAST asts) |]

haijiAST :: Quasi q => AST -> q Exp
haijiAST (Literal l) =
  runQ [e| return $(litE $ stringL $ T.unpack l) |]
haijiAST (Deref x) =
  runQ [e| do esc <- asks haijiEscape
              esc . toLT <$> $(deref x)
         |]
haijiAST (Condition p ts fs) =
  runQ [e| do cond <- $(deref p)
              if cond then $(haijiASTs ts) else $(maybe [e| return "" |] haijiASTs fs)
         |]
haijiAST (Foreach k xs loopBody elseBody) =
  runQ [e| do dicts <- $(deref xs)
              p <- ask
              let len = length dicts
              if 0 < len
              then return $ LT.concat
                            [ runReader $(haijiASTs loopBody)
                              HaijiParams { haijiEscape = haijiEscape p
                                          , haijiDict = haijiDict p `merge`
                                                        singleton x (Key :: Key $(litT . strTyLit $ show k)) `merge`
                                                        singleton (loopVariables len ix) (Key :: Key "loop")
                                                        }
                            | (ix, x) <- zip [0..] dicts
                            ]
              else $(maybe [e| return "" |] haijiASTs elseBody)
         |]
haijiAST (Include file) = haijiImportFile file
haijiAST (Raw raw) = runQ [e| return raw |]
haijiAST (Extends _file) = undefined
haijiAST (Block _base _name _scoped _body) = undefined
haijiAST (Comment _) = runQ [e| return ""|]

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

data HaijiParams dict = HaijiParams { haijiDict :: dict, haijiEscape :: LT.Text -> LT.Text }

deref :: Quasi q => Variable -> q Exp
deref (Simple v) =
  runQ [e| retrieve <$> asks haijiDict <*> return (Key :: Key $(litT . strTyLit $ show v)) |]
deref (Attribute v f) =
  runQ [e| retrieve <$> $(deref v) <*> return (Key :: Key $(litT . strTyLit $ show f)) |]
deref (At v ix) =
  runQ [e| (!! ix) <$> $(deref v) |]
