{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Text.Haiji.TH
       ( haiji
       , haijiFile
       ) where

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
import Text.Haiji.Syntax
import Text.Haiji.Types
import GHC.TypeLits
import GHC.Records
import GHC.Exts (Constraint)





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
              let len = toInteger $ Prelude.length dicts
              if 0 < len
              then return $ LT.concat
                            [ runReader $(haijiASTs env parentBlock children loopBody)
                              (SetDict @"loop" (loopVariables len ix) $
                               SetDict @($(litT . strTyLit $ show k)) x p)
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
haijiAST  env  parentBlock  children (Set lhs rhs scopes) =
  runQ [e| do val <- $(eval rhs)
              d <- ask
              let newDict = SetDict @($(litT $ strTyLit $ show lhs)) val d
              return $ runReader $(haijiASTs env parentBlock children scopes) newDict
         |]

type family NotEqual (a :: Symbol) (b :: Symbol) :: Constraint where
  NotEqual a a = TypeError ('Text "Duplicate keys: " ':<>: 'ShowType a)
  NotEqual a b = ()

data SetDict (sym :: Symbol) val base = SetDict val base

instance {-# OVERLAPPING #-} HasField sym (SetDict sym val base) val where
  getField (SetDict val _)= val

instance {-# OVERLAPPABLE #-} (NotEqual sym sym', HasField sym base val) => HasField sym (SetDict sym' val' base) val where
  getField (SetDict _ base)= getField @sym base

data LoopVars = LoopVars Bool Integer Integer Bool Integer Integer Integer
instance HasField "first" LoopVars Bool where
  getField (LoopVars x _ _ _ _ _ _) = x
instance HasField "index" LoopVars Integer where
  getField (LoopVars _ x _ _ _ _ _) = x
instance HasField "index0" LoopVars Integer where
  getField (LoopVars _ _ x _ _ _ _) = x
instance HasField "last" LoopVars Bool where
  getField (LoopVars _ _ _ x _ _ _) = x
instance HasField "length" LoopVars Integer where
  getField (LoopVars _ _ _ _ x _ _) = x
instance HasField "revindex" LoopVars Integer where
  getField (LoopVars _ _ _ _ _ x _) = x
instance HasField "revindex0" LoopVars Integer where
  getField (LoopVars _ _ _ _ _ _ x) = x


loopVariables :: Integer -> Integer -> LoopVars
loopVariables len ix = LoopVars (ix == 0) (ix + 1) ix (ix == len - 1) len (len - ix) (len - ix - 1)

eval :: Quasi q => Expression -> q Exp
eval (Expression expression) = go expression where
  go :: Quasi q => Expr External level -> q Exp
  go (ExprLift e) = go e
  go (ExprIntegerLiteral n) = runQ [e| return (n :: Integer) |]
  go (ExprStringLiteral s) = let x = unwrap s in runQ [e| return (x :: T.Text) |]
  go (ExprBooleanLiteral b) = runQ [e| return b |]
  go (ExprVariable v) = runQ [e| getField @($(litT . strTyLit $ show v)) <$> ask |]
  go (ExprParen e) = go e
  go (ExprRange [stop]) = runQ [e| (\b -> [0..b-1]) <$> $(go stop) |]
  go (ExprRange [start, stop]) = runQ [e| (\a b -> [a..b-1]) <$> $(go start) <*> $(go stop) |]
  go (ExprRange [start, stop, step]) = runQ [e| (\a b c -> [a,a+c..b-1]) <$> $(go start) <*> $(go stop) <*> $(go step) |]
  go (ExprRange _) = error "unreachable"
  go (ExprAttributed e []) = go e
  go (ExprAttributed e attrs) = runQ [e| getField @($(litT $ strTyLit $ show $ Prelude.last attrs)) <$> $(go $ ExprAttributed e $ init attrs) |]
  go (ExprFiltered e []) = go e
  go (ExprFiltered e filters) = runQ [e| $(applyFilter (Prelude.last filters) $ ExprFiltered e $ init filters) |] where
    applyFilter FilterAbs e' = runQ [e| abs <$> $(go e') |]
    applyFilter FilterLength e' = runQ [e| toInteger . Prelude.length <$> $(go e') |]
  go (ExprPow e1 e2) = runQ [e| (^) <$> $(go e1) <*> $(go e2) |]
  go (ExprMul e1 e2) = runQ [e| (*) <$> $(go e1) <*> $(go e2) |]
  go (ExprDivF e1 e2) = runQ [e| (/) <$> $(go e1) <*> $(go e2) |]
  go (ExprDivI e1 e2) = runQ [e| div <$> $(go e1) <*> $(go e2) |]
  go (ExprMod e1 e2) = runQ [e| mod <$> $(go e1) <*> $(go e2) |]
  go (ExprAdd e1 e2) = runQ [e| (+) <$> $(go e1) <*> $(go e2) |]
  go (ExprSub e1 e2) = runQ [e| (-) <$> $(go e1) <*> $(go e2) |]
  go (ExprEQ e1 e2) = runQ [e| (==) <$> $(go e1) <*> $(go e2) |]
  go (ExprNEQ e1 e2) = runQ [e| (/=) <$> $(go e1) <*> $(go e2) |]
  go (ExprGT e1 e2) = runQ [e| (>) <$> $(go e1) <*> $(go e2) |]
  go (ExprGE e1 e2) = runQ [e| (>=) <$> $(go e1) <*> $(go e2) |]
  go (ExprLT e1 e2) = runQ [e| (<) <$> $(go e1) <*> $(go e2) |]
  go (ExprLE e1 e2) = runQ [e| (<=) <$> $(go e1) <*> $(go e2) |]
  go (ExprAnd e1 e2) = runQ [e| (&&) <$> $(go e1) <*> $(go e2) |]
  go (ExprOr e1 e2) = runQ [e| (||) <$> $(go e1) <*> $(go e2) |]
