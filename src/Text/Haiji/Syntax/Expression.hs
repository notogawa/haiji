{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
module Text.Haiji.Syntax.Expression
       ( Expression(..)
       , expression
       , Expr(..)
       ) where

import Prelude hiding (filter)
#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif
import Data.Attoparsec.Text
import Text.Haiji.Syntax.Identifier
import Text.Haiji.Syntax.Filter
{-
data Filter = Filter deriving Eq

instance Show Filter where
  show _ = ""
-}
data Filtered
data Attributed
data Level0

data Expr level where
  ExprParen :: Expr Filtered -> Expr Level0
  ExprVariable :: Identifier -> Expr Level0
  ExprAttributed :: Expr Level0 -> [Identifier] -> Expr Attributed
  ExprFiltered :: Expr Attributed -> [Filter] -> Expr Filtered

deriving instance Eq (Expr level)

instance Show (Expr phase) where
  show (ExprParen e) = '(' : shows e ")"
  show (ExprVariable v) = show v
  show (ExprAttributed e attrs) = shows e $ concat [ '.' : show a | a <- attrs ]
  show (ExprFiltered v filters) = shows v $ filters >>= show

exprParen :: Parser (Expr Level0)
exprParen = ExprParen <$> (char '(' *> exprFiltered <* char ')')

exprVariable :: Parser (Expr Level0)
exprVariable = ExprVariable <$> identifier

exprLevel0 :: Parser (Expr Level0)
exprLevel0 = choice [exprParen, exprVariable]

exprAttributed :: Parser (Expr Attributed)
exprAttributed = ExprAttributed <$> exprLevel0 <*> many' (skipSpace *> char '.' *> skipSpace *> identifier)

-- |
--
-- >>> import Control.Arrow (left)
-- >>> let eval = left (const "parse error") . parseOnly exprFiltered
-- >>> eval "foo|abs"
-- Right foo|abs
-- >>> eval "foo| abs"
-- Right foo|abs
-- >>> eval "foo |abs"
-- Right foo|abs
-- >>> eval "foo | abs"
-- Right foo|abs
exprFiltered :: Parser (Expr Filtered)
exprFiltered = ExprFiltered <$> exprAttributed <*> many' (skipSpace *> filter)

newtype Expression = Expression (Expr Filtered) deriving Eq

instance Show Expression where
  show (Expression e) = show e

-- |
--
-- >>> import Control.Arrow (left)
-- >>> let eval = left (const "parse error") . parseOnly expression
-- >>> eval "foo"
-- Right foo
-- >>> eval "(foo)"
-- Right (foo)
-- >>> eval "foo.bar"
-- Right foo.bar
-- >>> eval "(foo).bar"
-- Right (foo).bar
-- >>> eval "(foo.bar)"
-- Right (foo.bar)
-- >>> eval "foo.b}}ar"
-- Right foo.b
-- >>> eval "foo.b ar"
-- Right foo.b
-- >>> eval "foo.b }ar"
-- Right foo.b
-- >>> eval " foo.bar"
-- Left "parse error"
-- >>> eval "foo.  bar"
-- Right foo.bar
-- >>> eval "foo  .bar"
-- Right foo.bar
-- >>> eval "foo.bar  "
-- Right foo.bar
-- >>> eval "foo.bar  "
-- Right foo.bar
-- >>> eval "foo.bar.baz"
-- Right foo.bar.baz
--
expression :: Parser Expression
expression = Expression <$> exprFiltered -- many (skipSpace >> char '|' >> skipSpace >> filter)
