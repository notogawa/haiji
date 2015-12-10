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
-- >>> let eval = either (error "parse error") id . parseOnly identifier
-- >>> eval "a"
-- a
-- >>> eval "ab"
-- ab
-- >>> eval "A"
-- A
-- >>> eval "Ab"
-- Ab
-- >>> eval "_"
-- _
-- >>> eval "_a"
-- _a
-- >>> eval "_1"
-- _1
-- >>> eval "__"
-- __
-- >>> eval "_ "
-- _
-- >>> eval " _"
-- *** Exception: parse error
-- >>> eval "and"
-- *** Exception: parse error
-- >>> eval "1"
-- *** Exception: parse error
-- >>> eval "1b"
-- *** Exception: parse error
-- >>> eval "'x"
-- *** Exception: parse error
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
