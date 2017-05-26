{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
module Text.Haiji.TH
       ( haiji
       , haijiFile
       , key
       ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif
import Control.Monad.Trans.Reader
import Data.Dynamic
import qualified Data.HashMap.Strict as M
import Data.Maybe
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax hiding (lift)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Text.Haiji.Parse
import Text.Haiji.Syntax
import Text.Haiji.Dictionary
import Text.Haiji.Types

-- | QuasiQuoter to generate a Haiji template
haiji :: Environment -> QuasiQuoter
haiji env = QuasiQuoter { quoteExp = haijiExp env
                        , quotePat = undefined
                        , quoteType = undefined
                        , quoteDec = undefined
                        }

-- | Generate a Haiji template from external file
haijiFile :: Quasi q => Environment -> FilePath -> q Exp
haijiFile env file = runQ (parseFile file) >>= haijiTemplate env

haijiExp :: Quasi q => Environment -> String -> q Exp
haijiExp env str = runQ (runIO $ parseString str) >>= haijiTemplate env

-- | Generate a dictionary with single item
key :: QuasiQuoter
key = QuasiQuoter { quoteExp = \k -> [e| \v -> singleton v (Key :: Key $(litT . strTyLit $ k)) |]
                  , quotePat = undefined
                  , quoteType = undefined
                  , quoteDec = undefined
                  }

haijiTemplate :: Quasi q => Environment -> Jinja2 -> q Exp
haijiTemplate env tmpl = runQ [e| Template $(haijiASTs env Nothing (jinja2Child tmpl) (jinja2Base tmpl)) |]

haijiASTs :: Quasi q => Environment -> Maybe [AST 'Fully] -> [AST 'Fully] -> [AST 'Fully] -> q Exp
haijiASTs env parentBlock children asts = runQ [e| LT.concat <$> sequence $(listE $ map (haijiAST env parentBlock children) asts) |]

haijiAST :: Quasi q => Environment -> Maybe [AST 'Fully] -> [AST 'Fully] -> AST 'Fully -> q Exp
haijiAST _env _parentBlock _children (Literal l) =
  runQ [e| return $(litE $ stringL $ T.unpack l) |]
haijiAST  env _parentBlock _children (Eval x) =
  if autoEscape env
  then runQ [e| (`escapeBy` htmlEscape) . toLT <$> $(eval x) |]
  else runQ [e| (`escapeBy` rawEscape) . toLT <$> $(eval x) |]
haijiAST  env  parentBlock  children (Condition p ts fs) =
  runQ [e| do cond <- $(eval p)
              if cond
              then $(haijiASTs env parentBlock children ts)
              else $(maybe [e| return "" |] (haijiASTs env parentBlock children) fs)
         |]
haijiAST  env  parentBlock  children (Foreach k xs loopBody elseBody) =
  runQ [e| do dicts <- $(eval xs)
              p <- ask
              let len = length dicts
              if 0 < len
              then return $ LT.concat
                            [ runReader $(haijiASTs env parentBlock children loopBody)
                              (p `merge`
                               singleton x (Key :: Key $(litT . strTyLit $ show k)) `merge`
                               singleton (loopVariables len ix) (Key :: Key "loop")
                              )
                            | (ix, x) <- zip [0..] dicts
                            ]
              else $(maybe [e| return "" |] (haijiASTs env parentBlock children) elseBody)
         |]
haijiAST _env _parentBlock _children (Raw raw) = runQ [e| return raw |]
haijiAST _env _parentBlock _children (Base _asts) = undefined
haijiAST  env  parentBlock  children (Block _base name _scoped body) =
  case listToMaybe [ b | Block _ n _ b <- children, n == name ] of
    Nothing    -> haijiASTs env parentBlock children body
    Just child -> haijiASTs env (Just body) children child
haijiAST  env  parentBlock  children Super = maybe (error "invalid super()") (haijiASTs env Nothing children) parentBlock
haijiAST _env _parentBlock _children (Comment _) = runQ [e| return "" |]

loopVariables :: Int -> Int -> Dict '["first" :-> Bool, "index" :-> Int, "index0" :-> Int, "last" :-> Bool, "length" :-> Int, "revindex" :-> Int, "revindex0" :-> Int]
loopVariables len ix = Dict $ M.fromList [ ("first", toDyn (ix == 0))
                                         , ("index", toDyn (ix + 1))
                                         , ("index0", toDyn ix)
                                         , ("last", toDyn (ix == len - 1))
                                         , ("length", toDyn len)
                                         , ("revindex", toDyn (len - ix))
                                         , ("revindex0", toDyn (len - ix - 1))
                                         ]

eval :: Quasi q => Expression -> q Exp
eval (Expression var _) = deref var

deref :: Quasi q => Variable -> q Exp
deref (VariableBase v) =
  runQ [e| retrieve <$> ask <*> return (Key :: Key $(litT . strTyLit $ show v)) |]
deref (VariableAttribute v f) =
  runQ [e| retrieve <$> $(deref v) <*> return (Key :: Key $(litT . strTyLit $ show f)) |]
