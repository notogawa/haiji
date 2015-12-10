{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
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
import Text.Haiji.Types

readTemplateFile :: Environments -> FilePath -> IO (Tmpl JSON.Value)
readTemplateFile env file = unsafeTmpl env <$> parseFile file

unsafeTmpl :: Environments -> Template -> Tmpl JSON.Value
unsafeTmpl env tmpl = Tmpl $ haijiASTs env Nothing (templateChild tmpl) (templateBase tmpl)

haijiASTs :: Environments -> Maybe [AST 'Loaded] -> [AST 'Loaded] -> [AST 'Loaded] -> Reader JSON.Value LT.Text
haijiASTs env parentBlock children asts = LT.concat <$> sequence (map (haijiAST env parentBlock children) asts)

haijiAST :: Environments -> Maybe [AST 'Loaded] -> [AST 'Loaded] -> AST 'Loaded -> Reader JSON.Value LT.Text
haijiAST _env _parentBlock _children (Literal l) =
  return $ LT.fromStrict l
haijiAST  env _parentBlock _children (Eval x) =
  do let esc = if autoEscape env then htmlEscape else rawEscape
     obj <- eval x
     case obj of
       JSON.String s -> return $ (`escapeBy` esc) $ toLT s
       JSON.Number n -> case floatingOrInteger n of
         Left  r -> const undefined (r :: Double)
         Right i -> return $ (`escapeBy` esc) $ toLT (i :: Integer)
       _ -> undefined
haijiAST  env  parentBlock  children (Condition p ts fs) =
  do JSON.Bool cond <- eval p
     if cond
     then haijiASTs env parentBlock children ts
     else maybe (return "") (haijiASTs env parentBlock children) fs
haijiAST  env  parentBlock  children (Foreach k xs loopBody elseBody) =
  do JSON.Array dicts <- eval xs
     p <- ask
     let len = V.length dicts
     if 0 < len
     then return $ LT.concat
                   [ runReader (haijiASTs env parentBlock children loopBody)
                     (let JSON.Object obj = p
                      in  JSON.Object
                          $ HM.insert "loop" (loopVariables len ix)
                          $ HM.insert (T.pack $ show k) x
                          $ obj)
                   | (ix, x) <- zip [0..] (V.toList dicts)
                   ]
     else maybe (return "") (haijiASTs env parentBlock children) elseBody

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

eval :: Expr -> Reader JSON.Value JSON.Value
eval (Var v) = deref v
eval (Fun _) = undefined

deref :: Variable -> Reader JSON.Value JSON.Value
deref (VariableBase v) = do
  dict <- ask
  maybe (error $ show (VariableBase v, dict)) return $ JSON.parseMaybe (JSON.withObject (show v) (JSON..: (T.pack $ show v))) dict
deref (VariableAttribute v f) = do
  dict <- deref v
  maybe (error "2") return $ JSON.parseMaybe (JSON.withObject (show f) (JSON..: (T.pack $ show f))) dict
