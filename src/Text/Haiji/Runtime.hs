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
       JSON.Bool b   -> return $ (`escapeBy` esc) $ toLT b
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
haijiAST  env  parentBlock  children (Set lhs rhs scopes) =
  do val <- eval rhs
     p <- ask
     return $ runReader (haijiASTs env parentBlock children scopes)
       (let JSON.Object obj = p in  JSON.Object $ HM.insert (T.pack $ show lhs) val obj)

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
eval (Expression expression) = go expression where
  go :: Expr level -> Reader JSON.Value JSON.Value
  go (ExprIntegerLiteral n) = return $ JSON.Number $ scientific (toEnum n) 0
  go (ExprBooleanLiteral b) = return $ JSON.Bool b
  go (ExprVariable v) = either error id . JSON.parseEither (JSON.withObject (show v) (JSON..: (T.pack $ show v))) <$> ask
  go (ExprParen e) = go e
  go (ExprAttributed e []) = go e
  go (ExprAttributed e attrs) = either error id . JSON.parseEither (JSON.withObject (show $ last attrs) (JSON..: (T.pack $ show $ last attrs))) <$> go (ExprAttributed e $ init attrs)
  go (ExprFiltered e []) = go e
  go (ExprFiltered e filters) = applyFilter (last filters) $ ExprFiltered e $ init filters where
    applyFilter FilterAbs e' = either error id . JSON.parseEither (JSON.withScientific "abs" (return . JSON.Number . abs)) <$> go e'
    applyFilter FilterLength e' = either error id . JSON.parseEither (JSON.withArray "length" (return . JSON.Number . flip scientific 0 . toEnum . V.length)) <$> go e'

  go (ExprPow e []) = go e
  go (ExprPow e ps) = do
    v1 <- either error id . JSON.parseEither (JSON.withScientific "lhs of (**)" return) <$> go (ExprPow e $ init ps)
    v2 <- either error id . JSON.parseEither (JSON.withScientific "rhs of (**)" return) <$> go (last ps)
    case (floatingOrInteger v1 :: Either Float Integer, floatingOrInteger v2 :: Either Float Int) of
      (Right l, Right r) -> return $ JSON.Number $ scientific (l ^ r) 0
      _                  -> error "(**)"
