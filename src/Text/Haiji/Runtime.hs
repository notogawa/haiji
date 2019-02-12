{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
-- |
-- Module      : Text.Haiji.Runtime
-- Copyright   : 2015 Noriyuki OHKAWA
-- License     : BSD3
--
-- Maintainer  : n.ohkawa@gmail.com
-- Stability   : experimental
-- Portability : portable
module Text.Haiji.Runtime
       ( readTemplateFile
       ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif
import Control.Monad.Trans.Reader
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as V
import Text.Haiji.Parse
import Text.Haiji.Syntax
import Text.Haiji.Types

-- | Dynamically template loader (for template development use)
readTemplateFile :: Environment -> FilePath -> IO (Template JSON.Value)
readTemplateFile env file = unsafeTemplate env <$> parseFile file

unsafeTemplate :: Environment -> Jinja2 -> Template JSON.Value
unsafeTemplate env tmpl = Template $ haijiASTs env Nothing (jinja2Child tmpl) (jinja2Base tmpl)

haijiASTs :: Environment -> Maybe [AST 'Fully] -> [AST 'Fully] -> [AST 'Fully] -> Reader JSON.Value LT.Text
haijiASTs env parentBlock children asts = LT.concat <$> mapM (haijiAST env parentBlock children) asts

haijiAST :: Environment -> Maybe [AST 'Fully] -> [AST 'Fully] -> AST 'Fully -> Reader JSON.Value LT.Text
haijiAST _env _parentBlock _children (Literal l) =
  return $ LT.fromStrict l
haijiAST  env _parentBlock _children (Eval x) =
  do let esc = if autoEscape env then htmlEscape else rawEscape
     obj <- eval x
     case obj of
       JSON.String s -> return $ (`escapeBy` esc) $ toLT s
       JSON.Number n -> case floatingOrInteger (n :: Scientific) of
         Left  r -> let _ = (r :: Double) in error "invalid value type"
         Right i -> return $ (`escapeBy` esc) $ toLT (i :: Integer)
       _ -> undefined
haijiAST  env  parentBlock  children (Condition p ts fs) =
  do b <- eval p
     case b of
       JSON.Bool True  -> haijiASTs env parentBlock children ts
       JSON.Bool False -> maybe (return "") (haijiASTs env parentBlock children) fs
       _               -> error "invalid condition type"
haijiAST  env  parentBlock  children (Foreach k xs loopBody elseBody) =
  do arr <- eval xs
     case arr of
       JSON.Array dicts -> do
         p <- ask
         let len = V.length dicts
         if 0 < len
         then return $ LT.concat
              [ runReader (haijiASTs env parentBlock children loopBody)
                          (let JSON.Object obj = p
                           in  JSON.Object
                               $ HM.insert "loop" (loopVariables len ix)
                               $ HM.insert (T.pack $ show k) x obj)
              | (ix, x) <- zip [0..] (V.toList dicts)
              ]
         else maybe (return "") (haijiASTs env parentBlock children) elseBody
       _                -> error "invalid array type"
haijiAST _env _parentBlock _children (Raw raw) = return $ LT.pack raw
haijiAST _env _parentBlock _children (Base _asts) = undefined
haijiAST  env  parentBlock  children (Block _base name _scoped body) =
  case listToMaybe [ b | Block _ n _ b <- children, n == name ] of
    Nothing    -> haijiASTs env parentBlock children body
    Just child -> haijiASTs env (Just body) children child
haijiAST  env  parentBlock  children Super = maybe (error "invalid super()") (haijiASTs env Nothing children) parentBlock
haijiAST _env _parentBlock _children (Comment _) = return ""

loopVariables :: Int -> Int -> JSON.Value
loopVariables len ix = JSON.object [ "first"     JSON..= (ix == 0)
                                   , "index"     JSON..= (ix + 1)
                                   , "index0"    JSON..= ix
                                   , "last"      JSON..= (ix == len - 1)
                                   , "length"    JSON..= len
                                   , "revindex"  JSON..= (len - ix)
                                   , "revindex0" JSON..= (len - ix - 1)
                                   ]

eval :: Expression -> Reader JSON.Value JSON.Value
eval (Expression var _) = deref var

deref :: Variable -> Reader JSON.Value JSON.Value
deref (VariableBase v) = do
  dict <- ask
  maybe (error $ show (VariableBase v, dict)) return $ JSON.parseMaybe (JSON.withObject (show v) (JSON..: (T.pack $ show v))) dict
deref (VariableAttribute v f) = do
  dict <- deref v
  maybe (error "2") return $ JSON.parseMaybe (JSON.withObject (show f) (JSON..: (T.pack $ show f))) dict
