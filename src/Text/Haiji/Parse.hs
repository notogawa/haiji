{-# LANGUAGE OverloadedStrings #-}
module Text.Haiji.Parse where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Text as T

data Variable = SimpleVariable T.Text
              | ObjectDotVariable Variable T.Text
--              | ArrayObjectVariable Variable T.Text
              | ArrayIndexVariable Variable Int
                deriving Eq

instance Show Variable where
    show (SimpleVariable v) = T.unpack v
    show (ObjectDotVariable v f) = shows v "." ++ T.unpack f
--    show (ArrayObjectVariable v f) = shows v "[\"" ++ T.unpack f ++ "\"]"
    show (ArrayIndexVariable v ix) = shows v "[" ++ show ix ++ "]"

data AST = Literal T.Text
         | Deref Variable
           deriving Eq

instance Show AST where
    show (Literal l) = T.unpack l
    show (Deref v) = "{{ " ++ shows v " }}"

parser :: Parser [AST]
parser = many $ choice [ literalParser
                       , derefParser
                       ]

literalParser :: Parser AST
literalParser = Literal <$> takeWhile1 (/= '{')

derefParser :: Parser AST
derefParser = do
  string "{{"
  skipSpace
  v <- variableParser
  skipSpace
  string "}}"
  return $ Deref v

variableParser :: Parser Variable
variableParser = ident >>= variableParser' . SimpleVariable where
    ident = takeTill (inClass " .[}")
    variableParser' v = do
      peek <- peekChar
      case peek of
        Nothing  -> return v
        Just '}' -> return v
        Just ' ' -> return v
        Just '.' -> do
          char '.'
          key <- ident
          variableParser' (ObjectDotVariable v key)
        Just '[' -> do
          char '['
          skipSpace
          ix <- decimal
          skipSpace
          char ']'
          variableParser' (ArrayIndexVariable v ix)
        _        -> fail "variableParser: invalid variable"
