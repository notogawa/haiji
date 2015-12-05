{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
module Text.Haiji.TH ( haiji, haijiFile, key ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif
import Control.Monad.Trans.Reader
import Data.Maybe
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax hiding (lift)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Text.Haiji.Parse
import Text.Haiji.Dictionary
import Text.Haiji.Types

haiji :: QuasiQuoter
haiji = QuasiQuoter { quoteExp = haijiExp
                    , quotePat = undefined
                    , quoteType = undefined
                    , quoteDec = undefined
                    }

haijiFile :: Quasi q => FilePath -> q Exp
haijiFile file = runQ (runIO $ parseFile file) >>= haijiTemplate

haijiExp :: Quasi q => String -> q Exp
haijiExp str = runQ (runIO $ parseString str) >>= haijiTemplate

key :: QuasiQuoter
key = QuasiQuoter { quoteExp = \k -> [e| \v -> singleton v (Key :: Key $(litT . strTyLit $ k)) |]
                  , quotePat = undefined
                  , quoteType = undefined
                  , quoteDec = undefined
                  }

haijiTemplate :: Quasi q => Template -> q Exp
haijiTemplate tmpl = haijiASTs Nothing (templateChild tmpl) (templateBase tmpl)

haijiASTs :: Quasi q => Maybe [AST 'Loaded] -> [AST 'Loaded] -> [AST 'Loaded] -> q Exp
haijiASTs parentBlock children asts = runQ [e| LT.concat <$> sequence $(listE $ map (haijiAST parentBlock children) asts) |]

haijiAST :: Quasi q => Maybe [AST 'Loaded] -> [AST 'Loaded] -> AST 'Loaded -> q Exp
haijiAST _parentBlock _children (Literal l) =
  runQ [e| return $(litE $ stringL $ T.unpack l) |]
haijiAST _parentBlock _children (Eval x) =
  runQ [e| do esc <- asks renderSettingsEscape
              (`escapeBy` esc) . toLT <$> $(eval x)
         |]
haijiAST  parentBlock  children (Condition p ts fs) =
  runQ [e| do cond <- $(eval p)
              if cond
              then $(haijiASTs parentBlock children ts)
              else $(maybe [e| return "" |] (haijiASTs parentBlock children) fs)
         |]
haijiAST  parentBlock  children (Foreach k xs loopBody elseBody) =
  runQ [e| do dicts <- $(eval xs)
              p <- ask
              let len = length dicts
              if 0 < len
              then return $ LT.concat
                            [ runReader $(haijiASTs parentBlock children loopBody)
                              p { renderSettingsDict = renderSettingsDict p `merge`
                                                       singleton x (Key :: Key $(litT . strTyLit $ show k)) `merge`
                                                       singleton (loopVariables len ix) (Key :: Key "loop")
                                }
                            | (ix, x) <- zip [0..] dicts
                            ]
              else $(maybe [e| return "" |] (haijiASTs parentBlock children) elseBody)
         |]
haijiAST _parentBlock _children (Raw raw) = runQ [e| return raw |]
haijiAST _parentBlock _children (Base _asts) = undefined
haijiAST  parentBlock  children (Block _base name _scoped body) =
  case listToMaybe [ b | Block _ n _ b <- children, n == name ] of
    Nothing    -> haijiASTs parentBlock children body
    Just child -> haijiASTs (Just body) children child
haijiAST  parentBlock  children Super = maybe (error "invalid super()") (haijiASTs Nothing children) parentBlock
haijiAST _parentBlock _children (Comment _) = runQ [e| return "" |]

loopVariables :: Int -> Int -> Dict '["first" :-> Bool, "index" :-> Int, "index0" :-> Int, "last" :-> Bool, "length" :-> Int, "revindex" :-> Int, "revindex0" :-> Int]
loopVariables len ix =
  Ext (Value (ix == 0)       :: "first"     :-> Bool) $
  Ext (Value (ix + 1)        :: "index"     :-> Int ) $
  Ext (Value ix              :: "index0"    :-> Int ) $
  Ext (Value (ix == len - 1) :: "last"      :-> Bool) $
  Ext (Value len             :: "length"    :-> Int ) $
  Ext (Value (len - ix)      :: "revindex"  :-> Int ) $
  Ext (Value (len - ix - 1)  :: "revindex0" :-> Int ) $
  Empty

eval :: Quasi q => Expr -> q Exp
eval (Var v) = deref v
eval (Fun f) = undefined

deref :: Quasi q => Variable -> q Exp
deref (Simple v) =
  runQ [e| retrieve <$> asks renderSettingsDict <*> return (Key :: Key $(litT . strTyLit $ show v)) |]
deref (Attribute v f) =
  runQ [e| retrieve <$> $(deref v) <*> return (Key :: Key $(litT . strTyLit $ show f)) |]
deref (At v ix) =
  runQ [e| (!! ix) <$> $(deref v) |]
