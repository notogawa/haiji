module Text.Haiji.Syntax.Expression
       ( Expression(..)
       , expression
       ) where

import Data.Attoparsec.Text
import Text.Haiji.Syntax.Variable

data Filter = Filter deriving Eq

instance Show Filter where
  show _ = ""

data Expression = Expression Variable [Filter] deriving Eq

instance Show Expression where
  show (Expression var fs) = show var ++ concat [ '|' : show f | f <- fs ]

expression :: Parser Expression
expression = Expression <$> variable <*> return [] -- many (skipSpace >> char '|' >> skipSpace >> filter)
