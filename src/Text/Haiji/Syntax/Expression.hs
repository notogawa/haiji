{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
module Text.Haiji.Syntax.Expression
       ( Expression(..)
       , expression
       , Expr(..)
       , External
       ) where

import Prelude hiding (filter)
#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif
import Data.Attoparsec.Text
import Data.Bool
import Data.List hiding (filter)
import Data.Scientific
import Text.Haiji.Syntax.Identifier
import Text.Haiji.Syntax.Filter
import Text.Haiji.Syntax.Literal

-- $setup
-- >>> import Control.Arrow (left)

data Z
data S a

type Level0 = Z
type Level1 = S Level0
type Level2 = S Level1
type Level3 = S Level2
type Level4 = S Level3
type Level5 = S Level4
type Level6 = S Level5
type Level7 = S Level6
type Level8 = S Level7

type LevelMax = Level8

data MulDiv = Mul | DivF | DivI | Mod deriving Eq

instance Show MulDiv where
  show Mul = "*"
  show DivF = "/"
  show DivI = "//"
  show Mod = "%"

data AddSub = Add | Sub deriving Eq

instance Show AddSub where
  show Add = "+"
  show Sub = "-"

data Internal
data External

data Expr visibility level where
  ExprLift :: Expr visibility lv -> Expr visibility (S lv)
  ExprIntegerLiteral :: Integer -> Expr visibility Level0
  ExprStringLiteral :: StringLiteral -> Expr visibility Level0
  ExprBooleanLiteral :: Bool -> Expr visibility Level0
  ExprVariable :: Identifier -> Expr visibility Level0
  ExprParen :: Expr visibility LevelMax -> Expr visibility Level0
  ExprRange :: [Expr visibility LevelMax] -> Expr visibility Level0
  ExprAttributed :: Expr visibility Level0 -> [Identifier] -> Expr visibility Level1
  ExprFiltered :: Expr visibility Level1 -> [Filter] -> Expr visibility Level2
  ExprInternalPow :: Expr Internal Level2 -> [Expr Internal Level2] -> Expr Internal Level3
  ExprPow :: Expr External Level3 -> Expr External Level2 -> Expr External Level3
  ExprInternalMulDiv :: Expr Internal Level3 -> [(MulDiv, Expr Internal Level3)] -> Expr Internal Level4
  ExprMul :: Expr External Level4 -> Expr External Level3 -> Expr External Level4
  ExprDivF :: Expr External Level4 -> Expr External Level3 -> Expr External Level4
  ExprDivI :: Expr External Level4 -> Expr External Level3 -> Expr External Level4
  ExprMod :: Expr External Level4 -> Expr External Level3 -> Expr External Level4
  ExprInternalAddSub :: Expr Internal Level4 -> [(AddSub, Expr Internal Level4)] -> Expr Internal Level5
  ExprAdd :: Expr External Level5 -> Expr External Level4 -> Expr External Level5
  ExprSub :: Expr External Level5 -> Expr External Level4 -> Expr External Level5
  ExprEQ :: Expr visibility Level5 -> Expr visibility Level5 -> Expr visibility Level6
  ExprNEQ :: Expr visibility Level5 -> Expr visibility Level5 -> Expr visibility Level6
  ExprGT :: Expr visibility Level5 -> Expr visibility Level5 -> Expr visibility Level6
  ExprGE :: Expr visibility Level5 -> Expr visibility Level5 -> Expr visibility Level6
  ExprLT :: Expr visibility Level5 -> Expr visibility Level5 -> Expr visibility Level6
  ExprLE :: Expr visibility Level5 -> Expr visibility Level5 -> Expr visibility Level6
  ExprInternalAnd :: Expr Internal Level6 -> [Expr Internal Level6] -> Expr Internal Level7
  ExprAnd :: Expr External Level7 -> Expr External Level6 -> Expr External Level7
  ExprInternalOr :: Expr Internal Level7 -> [Expr Internal Level7] -> Expr Internal Level8
  ExprOr :: Expr External Level8 -> Expr External Level7 -> Expr External Level8

toExternal :: Expr Internal level -> Expr External level
toExternal (ExprLift e) = ExprLift $ toExternal e
toExternal (ExprIntegerLiteral n) = ExprIntegerLiteral n
toExternal (ExprStringLiteral n) = ExprStringLiteral n
toExternal (ExprBooleanLiteral b) = ExprBooleanLiteral b
toExternal (ExprVariable i) = ExprVariable i
toExternal (ExprParen e) = ExprParen $ toExternal e
toExternal (ExprRange args) = ExprRange $ map toExternal args
toExternal (ExprAttributed e attrs) = ExprAttributed (toExternal e) attrs
toExternal (ExprFiltered e filters) = ExprFiltered (toExternal e) filters
toExternal (ExprInternalPow e []) = ExprLift $ toExternal e
toExternal (ExprInternalPow e es) = ExprPow (toExternal (ExprInternalPow e $ init es)) (toExternal $ last es)
toExternal (ExprInternalMulDiv e []) = ExprLift $ toExternal e
toExternal (ExprInternalMulDiv e es) = case last es of
  (Mul , e') -> ExprMul  (toExternal (ExprInternalMulDiv e $ init es)) (toExternal e')
  (DivF, e') -> ExprDivF (toExternal (ExprInternalMulDiv e $ init es)) (toExternal e')
  (DivI, e') -> ExprDivI (toExternal (ExprInternalMulDiv e $ init es)) (toExternal e')
  (Mod , e') -> ExprMod  (toExternal (ExprInternalMulDiv e $ init es)) (toExternal e')
toExternal (ExprInternalAddSub e []) = ExprLift $ toExternal e
toExternal (ExprInternalAddSub e es) = case last es of
  (Add, e') -> ExprAdd (toExternal (ExprInternalAddSub e $ init es)) (toExternal e')
  (Sub, e') -> ExprSub (toExternal (ExprInternalAddSub e $ init es)) (toExternal e')
toExternal (ExprEQ e1 e2) = ExprEQ (toExternal e1) (toExternal e2)
toExternal (ExprNEQ e1 e2) = ExprNEQ (toExternal e1) (toExternal e2)
toExternal (ExprGT e1 e2) = ExprGT (toExternal e1) (toExternal e2)
toExternal (ExprGE e1 e2) = ExprGE (toExternal e1) (toExternal e2)
toExternal (ExprLT e1 e2) = ExprLT (toExternal e1) (toExternal e2)
toExternal (ExprLE e1 e2) = ExprLE (toExternal e1) (toExternal e2)
toExternal (ExprInternalAnd e []) = ExprLift $ toExternal e
toExternal (ExprInternalAnd e es) = ExprAnd (toExternal (ExprInternalAnd e $ init es)) (toExternal $ last es)
toExternal (ExprInternalOr e []) = ExprLift $ toExternal e
toExternal (ExprInternalOr e es) = ExprOr (toExternal (ExprInternalOr e $ init es)) (toExternal $ last es)

deriving instance Eq (Expr visibility level)

instance Show (Expr visibility phase) where
  show (ExprLift e) = show e
  show (ExprIntegerLiteral n) = show n
  show (ExprStringLiteral n) = show n
  show (ExprBooleanLiteral b) = if b then "true" else "false"
  show (ExprVariable v) = show v
  show (ExprParen e) = '(' : shows e ")"
  show (ExprRange args) = "range(" ++ intercalate ", " [ show e | e <- args ] ++ ")"
  show (ExprAttributed e attrs) = shows e $ concat [ '.' : show a | a <- attrs ]
  show (ExprFiltered v filters) = shows v $ filters >>= show
  show (ExprInternalPow e es) = intercalate " ** " $ map show $ e:es
  show (ExprPow e1 e2) = shows e1 " ** " ++ show e2
  show (ExprInternalMulDiv e es) = concat $ show e : concat [ [ ' ' : shows op " ", show e' ] | (op, e') <- es ]
  show (ExprMul e1 e2) = shows e1 " * " ++ show e2
  show (ExprDivF e1 e2) = shows e1 " / " ++ show e2
  show (ExprDivI e1 e2) = shows e1 " // " ++ show e2
  show (ExprMod e1 e2) = shows e1 " % " ++ show e2
  show (ExprInternalAddSub e es) = concat $ show e : concat [ [ ' ' : shows op " ", show e' ] | (op, e') <- es ]
  show (ExprAdd e1 e2) = shows e1 " + " ++ show e2
  show (ExprSub e1 e2) = shows e1 " - " ++ show e2
  show (ExprEQ e1 e2) = shows e1 " == " ++ show e2
  show (ExprNEQ e1 e2) = shows e1 " != " ++ show e2
  show (ExprGT e1 e2) = shows e1 " > " ++ show e2
  show (ExprGE e1 e2) = shows e1 " >= " ++ show e2
  show (ExprLT e1 e2) = shows e1 " < " ++ show e2
  show (ExprLE e1 e2) = shows e1 " <= " ++ show e2
  show (ExprInternalAnd e es) = intercalate " and " $ map show $ e:es
  show (ExprAnd e1 e2) = shows e1 " and " ++ show e2
  show (ExprInternalOr e es) = intercalate " or " $ map show $ e:es
  show (ExprOr e1 e2) = shows e1 " or " ++ show e2

-- |
--
-- >>> let eval = left (const "parse error") . parseOnly exprIntegerLiteral
-- >>> eval "1"
-- Right 1
-- >>> eval "2"
-- Right 2
exprIntegerLiteral :: Parser (Expr Internal Level0)
exprIntegerLiteral = either (error . (show :: Double -> String)) ExprIntegerLiteral . floatingOrInteger <$> Data.Attoparsec.Text.scientific

-- |
--
-- >>> let eval = left (const "parse error") . parseOnly exprStringLiteral
-- >>> eval "'test'"
-- Right 'test'
-- >>> eval "\"test\""
-- Right "test"
exprStringLiteral :: Parser (Expr Internal Level0)
exprStringLiteral = ExprStringLiteral <$> stringLiteral

-- |
--
-- >>> let eval = left (const "parse error") . parseOnly exprBooleanLiteral
-- >>> eval "true"
-- Right true
-- >>> eval "false"
-- Right false
exprBooleanLiteral :: Parser (Expr Internal Level0)
exprBooleanLiteral = ExprBooleanLiteral <$> choice [ string "true" *> return True, string "false" *> return False ]

exprVariable :: Parser (Expr Internal Level0)
exprVariable = ExprVariable <$> identifier

-- |
--
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
exprParen :: Parser (Expr Internal Level0)
exprParen = ExprParen <$> (char '(' *> skipSpace *> exprLevelMax <* skipSpace <* char ')')

-- |
--
-- >>> let eval = left (const "parse error") . parseOnly exprRange
-- >>> eval "range(1)"
-- Right range(1)
-- >>> eval "range (1)"
-- Right range(1)
-- >>> eval "range(1, 2)"
-- Right range(1, 2)
-- >>> eval "range (1,2)"
-- Right range(1, 2)
-- >>> eval "range(1 ,2 , 3)"
-- Right range(1, 2, 3)
exprRange :: Parser (Expr Internal Level0)
exprRange = ExprRange <$> args where
  args = do
    es <- string "range" *> skipSpace *> char '(' *> skipSpace *> (exprLevelMax `sepBy1` (skipSpace *> char ',' *> skipSpace)) <* skipSpace <* char ')'
    bool (fail "too many args") (return es) $ length es < 4

exprLevel0 :: Parser (Expr Internal Level0)
exprLevel0 = choice [ exprIntegerLiteral
                    , exprStringLiteral
                    , exprBooleanLiteral
                    , exprRange
                    , exprVariable
                    , exprParen
                    ]

exprAttributed :: Parser (Expr Internal Level1)
exprAttributed = ExprAttributed <$> exprLevel0 <*> many' (skipSpace *> char '.' *> skipSpace *> identifier)

exprLevel1 :: Parser (Expr Internal Level1)
exprLevel1 = choice [ exprAttributed
                    ]

-- |
--
-- >>> let eval = left (const "parse error") . parseOnly exprFiltered
-- >>> eval "foo|abs"
-- Right foo|abs
-- >>> eval "foo| abs"
-- Right foo|abs
-- >>> eval "foo |abs"
-- Right foo|abs
-- >>> eval "foo | abs"
-- Right foo|abs
exprFiltered :: Parser (Expr Internal Level2)
exprFiltered = ExprFiltered <$> exprLevel1 <*> many' (skipSpace *> filter)

exprLevel2 :: Parser (Expr Internal Level2)
exprLevel2 = choice [ exprFiltered
                    ]

-- |
--
-- >>> let eval = left (const "parse error") . parseOnly exprPow
-- >>> eval "1**2"
-- Right 1 ** 2
-- >>> eval "1**  2"
-- Right 1 ** 2
-- >>> eval "1  **2"
-- Right 1 ** 2
exprPow :: Parser (Expr Internal Level3)
exprPow = ExprInternalPow <$> exprLevel2 <*> many' (skipSpace *> string "**" *> skipSpace *> exprLevel2)

exprLevel3 :: Parser (Expr Internal Level3)
exprLevel3 = choice [ exprPow
                    ]

-- |
--
-- >>> let eval = left (const "parse error") . parseOnly exprMulDiv
-- >>> eval "1*2//3"
-- Right 1 * 2 // 3
-- >>> eval "1 * 2 // 3"
-- Right 1 * 2 // 3
-- >>> eval "1//2*3"
-- Right 1 // 2 * 3
-- >>> eval "1*2/3"
-- Right 1 * 2 / 3
-- >>> eval "1 * 2 % 3"
-- Right 1 * 2 % 3
-- >>> eval "1%2*3"
-- Right 1 % 2 * 3
exprMulDiv :: Parser (Expr Internal Level4)
exprMulDiv = ExprInternalMulDiv <$> exprLevel3 <*> many' ((,) <$> (skipSpace *> op) <*> (skipSpace *> exprLevel3)) where
  op = choice [ string "//" *> return DivI
              , string "/" *> return DivF
              , string "*" *> return Mul
              , string "%" *> return Mod
              ]

exprLevel4 :: Parser (Expr Internal Level4)
exprLevel4 = choice [ exprMulDiv
                    ]

-- |
--
-- >>> let eval = left (const "parse error") . parseOnly exprAddSub
-- >>> eval "1+2-3"
-- Right 1 + 2 - 3
-- >>> eval "1 + 2 - 3"
-- Right 1 + 2 - 3
-- >>> eval "1-2+3"
-- Right 1 - 2 + 3
exprAddSub :: Parser (Expr Internal Level5)
exprAddSub = ExprInternalAddSub <$> exprLevel4 <*> many' ((,) <$> (skipSpace *> op) <*> (skipSpace *> exprLevel4)) where
  op = choice [ string "+" *> return Add
              , string "-" *> return Sub
              ]

exprLevel5 :: Parser (Expr Internal Level5)
exprLevel5 = choice [ exprAddSub
                    ]

-- |
--
-- >>> let eval = left (const "parse error") . parseOnly exprEQ
-- >>> eval "1==1"
-- Right 1 == 1
-- >>> eval "1== 1"
-- Right 1 == 1
-- >>> eval "1 ==1"
-- Right 1 == 1
exprEQ :: Parser (Expr Internal Level6)
exprEQ = ExprEQ <$> exprLevel5 <*> (skipSpace *> string "==" *> skipSpace *> exprLevel5)

-- |
--
-- >>> let eval = left (const "parse error") . parseOnly exprNEQ
-- >>> eval "1!=1"
-- Right 1 != 1
-- >>> eval "1!= 1"
-- Right 1 != 1
-- >>> eval "1 !=1"
-- Right 1 != 1
exprNEQ :: Parser (Expr Internal Level6)
exprNEQ = ExprNEQ <$> exprLevel5 <*> (skipSpace *> string "!=" *> skipSpace *> exprLevel5)

-- |
--
-- >>> let eval = left (const "parse error") . parseOnly exprGT
-- >>> eval "1>1"
-- Right 1 > 1
-- >>> eval "1> 1"
-- Right 1 > 1
-- >>> eval "1 >1"
-- Right 1 > 1
exprGT :: Parser (Expr Internal Level6)
exprGT = ExprGT <$> exprLevel5 <*> (skipSpace *> string ">" *> skipSpace *> exprLevel5)

-- |
--
-- >>> let eval = left (const "parse error") . parseOnly exprGE
-- >>> eval "1>=1"
-- Right 1 >= 1
-- >>> eval "1>= 1"
-- Right 1 >= 1
-- >>> eval "1 >=1"
-- Right 1 >= 1
exprGE :: Parser (Expr Internal Level6)
exprGE = ExprGE <$> exprLevel5 <*> (skipSpace *> string ">=" *> skipSpace *> exprLevel5)

-- |
--
-- >>> let eval = left (const "parse error") . parseOnly exprLT
-- >>> eval "1<1"
-- Right 1 < 1
-- >>> eval "1< 1"
-- Right 1 < 1
-- >>> eval "1 <1"
-- Right 1 < 1
exprLT :: Parser (Expr Internal Level6)
exprLT = ExprLT <$> exprLevel5 <*> (skipSpace *> string "<" *> skipSpace *> exprLevel5)

-- |
--
-- >>> let eval = left (const "parse error") . parseOnly exprLE
-- >>> eval "1<=1"
-- Right 1 <= 1
-- >>> eval "1<= 1"
-- Right 1 <= 1
-- >>> eval "1 <=1"
-- Right 1 <= 1
exprLE :: Parser (Expr Internal Level6)
exprLE = ExprLE <$> exprLevel5 <*> (skipSpace *> string "<=" *> skipSpace *> exprLevel5)

exprLevel6 :: Parser (Expr Internal Level6)
exprLevel6 = choice [ exprEQ
                    , exprNEQ
                    , exprGE
                    , exprGT
                    , exprLE
                    , exprLT
                    , ExprLift <$> exprLevel5
                    ]

-- |
--
-- >>> let eval = left (const "parse error") . parseOnly exprAnd
-- >>> eval "true and false"
-- Right true and false
exprAnd :: Parser (Expr Internal Level7)
exprAnd = ExprInternalAnd <$> exprLevel6 <*> many' (skipSpace *> string "and" *> skipSpace *> exprLevel6)

exprLevel7 :: Parser (Expr Internal Level7)
exprLevel7 = choice [ exprAnd
                    , ExprLift <$> exprLevel6
                    ]

-- |
--
-- >>> let eval = left (const "parse error") . parseOnly exprOr
-- >>> eval "true or false"
-- Right true or false
exprOr :: Parser (Expr Internal Level8)
exprOr = ExprInternalOr <$> exprLevel7 <*> many' (skipSpace *> string "or" *> skipSpace *> exprLevel7)

exprLevel8 :: Parser (Expr Internal Level8)
exprLevel8 = choice [ exprOr
                    , ExprLift <$> exprLevel7
                    ]

exprLevelMax :: Parser (Expr Internal LevelMax)
exprLevelMax = exprLevel8

newtype Expression = Expression (Expr External LevelMax) deriving Eq

instance Show Expression where
  show (Expression e) = show e

-- |
--
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
expression = Expression . toExternal <$> exprLevelMax
