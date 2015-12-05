{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
module Text.Haiji.Dynamic ( unsafeTmpl ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif
import Control.Monad.Trans.Reader
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Maybe
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax hiding (lift)
import qualified Data.HashMap.Strict as HM
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as V
import Text.Haiji.Parse
import Text.Haiji.Types

unsafeTmpl :: Template -> Reader (RenderSettings JSON.Value) LT.Text
unsafeTmpl tmpl = haijiASTs Nothing (templateChild tmpl) (templateBase tmpl)

haijiASTs :: Maybe [AST 'Loaded] -> [AST 'Loaded] -> [AST 'Loaded] -> Reader (RenderSettings JSON.Value) LT.Text
haijiASTs parentBlock children asts = LT.concat <$> sequence (map (haijiAST parentBlock children) asts)

haijiAST :: Maybe [AST 'Loaded] -> [AST 'Loaded] -> AST 'Loaded -> Reader (RenderSettings JSON.Value) LT.Text
haijiAST _parentBlock _children (Literal l) =
  return $ LT.fromStrict l
haijiAST _parentBlock _children (Eval x) =
  do esc <- asks renderSettingsEscape
     obj <- eval x
     case obj of
       JSON.String s -> return $ (`escapeBy` esc) $ toLT s
       JSON.Number n -> case floatingOrInteger n of
         Left _  -> undefined
         Right n -> return $ (`escapeBy` esc) $ toLT (n :: Integer)
       _ -> undefined
haijiAST  parentBlock  children (Condition p ts fs) =
  do JSON.Bool cond <- eval p
     if cond
     then haijiASTs parentBlock children ts
     else maybe (return "") (haijiASTs parentBlock children) fs
haijiAST  parentBlock  children (Foreach k xs loopBody elseBody) =
  do JSON.Array dicts <- eval xs
     p <- ask
     let len = V.length dicts
     if 0 < len
     then return $ LT.concat
                   [ runReader (haijiASTs parentBlock children loopBody)
                     p { renderSettingsDict = let JSON.Object obj = renderSettingsDict p
                                              in  JSON.Object
                                                  $ HM.insert "loop" (loopVariables len ix)
                                                  $ HM.insert (T.pack $ show k) x
                                                  $ obj
                       }
                   | (ix, x) <- zip [0..] (V.toList dicts)
                   ]
     else maybe (return "") (haijiASTs parentBlock children) elseBody

haijiAST _parentBlock _children (Raw raw) = return $ LT.pack raw
haijiAST _parentBlock _children (Base _asts) = undefined
haijiAST  parentBlock  children (Block _base name _scoped body) =
  case listToMaybe [ b | Block _ n _ b <- children, n == name ] of
    Nothing    -> haijiASTs parentBlock children body
    Just child -> haijiASTs (Just body) children child
haijiAST  parentBlock  children Super = maybe (error "invalid super()") (haijiASTs Nothing children) parentBlock
haijiAST _parentBlock _children (Comment _) = return ""

loopVariables :: Int -> Int -> JSON.Value
loopVariables len ix = JSON.object [ "first"     JSON..= (ix == 0)
                                   , "index"     JSON..= (ix + 1)
                                   , "index0"    JSON..= ix
                                   , "last"      JSON..= (ix == len - 1)
                                   , "length"    JSON..= len
                                   , "revindex"  JSON..= (len - ix)
                                   , "revindex0" JSON..= (len - ix - 1)
                                   ]

eval :: Expr -> Reader (RenderSettings JSON.Value) JSON.Value
eval (Var v) = deref v
eval (Fun f) = undefined

deref :: Variable -> Reader (RenderSettings JSON.Value) JSON.Value
deref (Simple v) = do
  dict <- asks renderSettingsDict
  maybe (error $ show (Simple v, dict)) return $ JSON.parseMaybe (JSON.withObject (show v) (JSON..: (T.pack $ show v))) dict
deref (Attribute v f) = do
  dict <- deref v
  maybe (error "2") return $ JSON.parseMaybe (JSON.withObject (show f) (JSON..: (T.pack $ show f))) dict
deref (At v ix) = do
  dict <- deref v
  maybe (error "3") return $ JSON.parseMaybe (JSON.withArray "" (\a -> return $ a V.! ix)) dict
