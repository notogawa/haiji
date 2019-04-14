{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
module Text.Haiji.Syntax.Expression
       ( Expression(..)
       , expression
       , Expr(..)
       , MulDiv(..)
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

data Level0
data Level1
data Level2
data Level3
data Level4

data MulDiv = Mul | DivF | DivI deriving Eq

instance Show MulDiv where
  show Mul = "*"
  show DivF = "/"
  show DivI = "//"

data Expr level where
  ExprIntegerLiteral :: Int -> Expr Level0
  ExprBooleanLiteral :: Bool -> Expr Level0
  ExprVariable :: Identifier -> Expr Level0
  ExprParen :: Expr Level4 -> Expr Level0
  ExprAttributed :: Expr Level0 -> [Identifier] -> Expr Level1
  ExprFiltered :: Expr Level1 -> [Filter] -> Expr Level2
  ExprPow :: Expr Level2 -> [Expr Level2] -> Expr Level3
  ExprMulDiv :: Expr Level3 -> [(MulDiv, Expr Level3)] -> Expr Level4

{-
  ExprMul :: Expr Level0 -> Expr Level1 -> Expr Level1
  ExprDivF :: Expr Level0 -> Expr Level1 -> Expr Level1
  ExprDivI :: Expr Level0 -> Expr Level1 -> Expr Level1
  ExprAdd :: Expr Level1 -> Expr Level2 -> Expr Level2
  ExprSub :: Expr Level1 -> Expr Level2 -> Expr Level2
-}

deriving instance Eq (Expr level)

instance Show (Expr phase) where
  show (ExprIntegerLiteral n) = show n
  show (ExprBooleanLiteral b) = if b then "true" else "false"
  show (ExprVariable v) = show v
  show (ExprParen e) = '(' : shows e ")"
  show (ExprAttributed e attrs) = shows e $ concat [ '.' : show a | a <- attrs ]
  show (ExprFiltered v filters) = shows v $ filters >>= show
  show (ExprPow e ps) = concat $ show e : concat [ [ " ** ", show p ] | p <- ps ]
  show (ExprMulDiv e ps) = concat $ show e : concat [ [ ' ' : shows op " ", show p ] | (op, p) <- ps ]

-- |
--
-- >>> import Control.Arrow (left)
-- >>> let eval = left (const "parse error") . parseOnly exprIntegerLiteral
-- >>> eval "1"
-- Right 1
-- >>> eval "2"
-- Right 2
exprIntegerLiteral :: Parser (Expr Level0)
exprIntegerLiteral = either (error . (show :: Double -> String)) ExprIntegerLiteral . floatingOrInteger <$> Data.Attoparsec.Text.scientific

-- |
--
-- >>> import Control.Arrow (left)
-- >>> let eval = left (const "parse error") . parseOnly exprBooleanLiteral
-- >>> eval "true"
-- Right true
-- >>> eval "false"
-- Right false
exprBooleanLiteral :: Parser (Expr Level0)
exprBooleanLiteral = ExprBooleanLiteral <$> choice [ string "true" *> return True, string "false" *> return False ]

exprVariable :: Parser (Expr Level0)
exprVariable = ExprVariable <$> identifier

-- |
--
-- >>> import Control.Arrow (left)
-- >>> let eval = left (const "parse error") . parseOnly exprParen
-- >>> eval "(1)"
-- Right (1)
-- >>> eval "(foo)"
-- Right (foo)
-- >>> eval "(true)"
-- Right (true)
-- >>> eval "(foo )"
-- Right (foo)
-- >>> eval "( foo)"
-- Right (foo)
exprParen :: Parser (Expr Level0)
exprParen = ExprParen <$> (char '(' *> skipSpace *> exprLevel4 <* skipSpace <* char ')')

exprLevel0 :: Parser (Expr Level0)
exprLevel0 = choice [ exprIntegerLiteral
                    , exprBooleanLiteral
                    , exprVariable
                    , exprParen
                    ]

exprAttributed :: Parser (Expr Level1)
exprAttributed = ExprAttributed <$> exprLevel0 <*> many' (skipSpace *> char '.' *> skipSpace *> identifier)

{-
exprMul :: Parser (Expr Level1)
exprMul = ExprMul <$> exprLevel0 <*> (skipSpace *> string "*" *> skipSpace *> exprLevel1)

exprDivF :: Parser (Expr Level1)
exprDivF = ExprDivF <$> exprLevel0 <*> (skipSpace *> string "/" *> skipSpace *> exprLevel1)

exprDivI :: Parser (Expr Level1)
exprDivI = ExprDivI <$> exprLevel0 <*> (skipSpace *> string "/" *> skipSpace *> exprLevel1)

exprLift0To1 :: Parser (Expr Level1)
exprLift0To1 = ExprLift0To1 <$> exprLevel0
-}

exprLevel1 :: Parser (Expr Level1)
exprLevel1 = choice [ exprAttributed
                    ]

{-
exprAdd :: Parser (Expr Level2)
exprAdd = ExprAdd <$> exprLevel1 <*> (skipSpace *> string "+" *> skipSpace *> exprLevel2)

exprSub :: Parser (Expr Level2)
exprSub = ExprAdd <$> exprLevel1 <*> (skipSpace *> string "-" *> skipSpace *> exprLevel2)

exprLift1To2 :: Parser (Expr Level2)
exprLift1To2 = ExprLift1To2 <$> exprLevel1
-}

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
exprFiltered :: Parser (Expr Level2)
exprFiltered = ExprFiltered <$> exprLevel1 <*> many' (skipSpace *> filter)

exprLevel2 :: Parser (Expr Level2)
exprLevel2 = choice [ exprFiltered
                    ]

-- |
--
-- >>> import Control.Arrow (left)
-- >>> let eval = left (const "parse error") . parseOnly exprPow
-- >>> eval "1**2"
-- Right 1 ** 2
-- >>> eval "1**  2"
-- Right 1 ** 2
-- >>> eval "1  **2"
-- Right 1 ** 2
exprPow :: Parser (Expr Level3)
exprPow = ExprPow <$> exprLevel2 <*> many' (skipSpace *> string "**" *> skipSpace *> exprLevel2)

exprLevel3 :: Parser (Expr Level3)
exprLevel3 = choice [ exprPow
                    ]

-- |
--
-- >>> import Control.Arrow (left)
-- >>> let eval = left (const "parse error") . parseOnly exprMulDiv
-- >>> eval "1*2//3"
-- Right 1 * 2 // 3
-- >>> eval "1 * 2 // 3"
-- Right 1 * 2 // 3
-- >>> eval "1//2*3"
-- Right 1 // 2 * 3
-- >>> eval "1*2/3"
-- Right 1 * 2 / 3
exprMulDiv :: Parser (Expr Level4)
exprMulDiv = ExprMulDiv <$> exprLevel3 <*> many' ((,) <$> (skipSpace *> op) <*> (skipSpace *> exprLevel3)) where
  op = choice [ string "//" *> return DivI
              , string "/" *> return DivF
              , string "*" *> return Mul
              ]

exprLevel4 :: Parser (Expr Level4)
exprLevel4 = choice [ exprMulDiv
                    ]

newtype Expression = Expression (Expr Level4) deriving Eq

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
expression = Expression <$> exprLevel4
