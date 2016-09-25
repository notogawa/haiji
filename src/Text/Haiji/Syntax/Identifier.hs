module Text.Haiji.Syntax.Identifier
       ( Identifier
       , identifier
       ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.String

newtype Identifier = Identifier { unIdentifier :: String } deriving Eq

instance Show Identifier where
  show = unIdentifier

instance IsString Identifier where
  fromString = Identifier

-- | python identifier
--
-- https://docs.python.org/2.7/reference/lexical_analysis.html#identifiers
--
-- >>> import Control.Arrow (left)
-- >>> let eval = left (const "parse error") . parseOnly identifier
-- >>> eval "a"
-- Right a
-- >>> eval "ab"
-- Right ab
-- >>> eval "A"
-- Right A
-- >>> eval "Ab"
-- Right Ab
-- >>> eval "_"
-- Right _
-- >>> eval "_a"
-- Right _a
-- >>> eval "_1"
-- Right _1
-- >>> eval "__"
-- Right __
-- >>> eval "_ "
-- Right _
-- >>> eval " _"
-- Left "parse error"
-- >>> eval "and"
-- Left "parse error"
-- >>> eval "1"
-- Left "parse error"
-- >>> eval "1b"
-- Left "parse error"
-- >>> eval "'x"
-- Left "parse error"
--
identifier :: Parser Identifier
identifier = do
  h <- letter <|> char '_'
  ts <- many (letter <|> digit <|> char '_')
  let candidate = h : ts
  when (candidate `elem` keywords) $ fail "identifier"
  return $ Identifier candidate

-- | python keywords
--
-- https://docs.python.org/2.7/reference/lexical_analysis.html#keywords
--
keywords :: [String]
keywords = words "and       del       from      not       while \
                 \as        elif      global    or        with  \
                 \assert    else      if        pass      yield \
                 \break     except    import    print           \
                 \class     exec      in        raise           \
                 \continue  finally   is        return          \
                 \def       for       lambda    try             "
