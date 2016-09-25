module Text.Haiji.Syntax.Variable
       ( Variable(..)
       , variable
       ) where

import Data.Attoparsec.Text
import Text.Haiji.Syntax.Identifier

data Variable = VariableBase Identifier
              | VariableAttribute Variable Identifier
              deriving Eq

instance Show Variable where
  show (VariableBase var) = show var
  show (VariableAttribute var attr) = shows var "." ++ show attr

-- |
--
-- >>> import Control.Arrow (left)
-- >>> let eval = left (const "parse error") . parseOnly variable
-- >>> eval "foo"
-- Right foo
-- >>> eval "foo.bar"
-- Right foo.bar
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
variable :: Parser Variable
variable = identifier >>= go . VariableBase where
  go var = do
    skipSpace
    peek <- peekChar
    case peek of
      Nothing  -> return var
      Just '}' -> return var
      Just ' ' -> return var
      Just '.' -> char '.' >> skipSpace >> identifier >>= go . VariableAttribute var
      _        -> return var
