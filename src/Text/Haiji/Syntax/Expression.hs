{-# LANGUAGE OverloadedStrings #-}
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
import Data.Scientific
import Text.Haiji.Syntax.Identifier
import Text.Haiji.Syntax.Filter

data Filtered
data Attributed
data Level0

data Expr level where
  ExprIntegerLiteral :: Int -> Expr Level0
  ExprBooleanLiteral :: Bool -> Expr Level0
  ExprVariable :: Identifier -> Expr Level0
  ExprParen :: Expr Filtered -> Expr Level0
  ExprAttributed :: Expr Level0 -> [Identifier] -> Expr Attributed
  ExprFiltered :: Expr Attributed -> [Filter] -> Expr Filtered

deriving instance Eq (Expr level)

instance Show (Expr phase) where
  show (ExprIntegerLiteral n) = show n
  show (ExprBooleanLiteral b) = if b then "true" else "false"
  show (ExprVariable v) = show v
  show (ExprParen e) = '(' : shows e ")"
  show (ExprAttributed e attrs) = shows e $ concat [ '.' : show a | a <- attrs ]
  show (ExprFiltered v filters) = shows v $ filters >>= show

exprIntegerLiteral :: Parser (Expr Level0)
exprIntegerLiteral = either (error . (show :: Double -> String)) ExprIntegerLiteral . floatingOrInteger <$> Data.Attoparsec.Text.scientific

exprBooleanLiteral :: Parser (Expr Level0)
exprBooleanLiteral = ExprBooleanLiteral <$> choice [ string "true" *> return True, string "false" *> return False ]

exprVariable :: Parser (Expr Level0)
exprVariable = ExprVariable <$> identifier

exprParen :: Parser (Expr Level0)
exprParen = ExprParen <$> (char '(' *> exprFiltered <* char ')')

exprLevel0 :: Parser (Expr Level0)
exprLevel0 = choice [ exprIntegerLiteral, exprBooleanLiteral, exprVariable, exprParen ]

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
-- >>> eval "1"
-- Right 1
-- >>> eval "true"
-- Right true
-- >>> eval "false"
-- Right false
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
