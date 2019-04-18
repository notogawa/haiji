module Text.Haiji.Syntax.Literal.String
       ( StringLiteral
       , stringLiteral
       , unwrap
       ) where

import Data.Attoparsec.Text hiding (string)

-- $setup
-- >>> import Control.Arrow (left)

data StringLiteral = SingleQuotedStringLiteral String
                   | DoubleQuotedStringLiteral String
                   deriving Eq

unwrap :: StringLiteral -> String
unwrap (SingleQuotedStringLiteral s) = s
unwrap (DoubleQuotedStringLiteral s) = s

instance Show StringLiteral where
  show (SingleQuotedStringLiteral str) = '\'' : (str >>= escape) ++ "'"  where
    escape c = maybe [c] id $ lookup c $ ('\'', "\\'") : requireEscape
  show (DoubleQuotedStringLiteral str) = '"' : (str >>= escape) ++ "\"" where
    escape c = maybe [c] id $ lookup c $ ('"', "\\\"") : requireEscape

-- |
--
-- >>> let eval = left (const "parse error") . parseOnly stringLiteral
-- >>> eval "'test'"
-- Right 'test'
-- >>> eval "\"test\""
-- Right "test"
-- >>> eval "'\\\'\"'"
-- Right '\'"'
-- >>> eval "\"\'\\\"\""
-- Right "'\""
stringLiteral :: Parser StringLiteral
stringLiteral = choice [ char '\'' *> (SingleQuotedStringLiteral <$> quotedBy '\'')
                       , char '"' *> (DoubleQuotedStringLiteral <$> quotedBy '"')
                       ]

requireUnescape :: [(Char, Char)]
requireUnescape = [ ('n', '\n')
                  , ('r', '\r')
                  , ('b', '\b')
                  , ('v', '\v')
                  , ('0', '\0')
                  , ('t', '\t')
                  ]

requireEscape :: [(Char, String)]
requireEscape = [ (b, ['\\', a]) | (a, b) <- requireUnescape ]

quotedBy :: Char -> Parser String
quotedBy = manyTill contents . char where
  contents :: Parser Char
  contents = do
    c <- anyChar
    case c of
      '\\' -> do
        escaped <- anyChar
        return $ maybe escaped id $ lookup escaped requireUnescape
      _ -> return c
