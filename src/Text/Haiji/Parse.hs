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
         | Condition Variable [AST] (Maybe [AST])
         | Foreach T.Text Variable [AST]
           deriving Eq

instance Show AST where
    show (Literal l) = T.unpack l
    show (Deref v) = "{{ " ++ shows v " }}"
    show (Condition p ts mfs) = "{% if " ++ show p ++ "%}" ++
                                concatMap show ts ++
                                maybe "" (\fs -> "{% else %}" ++ concatMap show fs) mfs ++
                                "{% endif %}"
    show (Foreach x xs asts) = "{% for " ++ show x ++ " in " ++ show xs ++ "%}" ++
                               concatMap show asts ++
                               "{% endfor %}"

parser :: Parser [AST]
parser = many $ choice [ literalParser
                       , derefParser
                       , conditionParser
                       , loopParser
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

skipTrailingWhitespace :: Parser ()
skipTrailingWhitespace = option () $ skipWhile isHorizontalSpace >> endOfLine

statement f = do
  string "{%"
  skipSpace
  x <- f
  skipSpace
  string "%}"
  return x

conditionParser :: Parser AST
conditionParser = do
  cond <- statement $ string "if" >> skipSpace >> variableParser
  skipTrailingWhitespace
  ifbody <- parser
  skipTrailingWhitespace
  statement $ string "endif" <|> string "else"
  skipTrailingWhitespace
  elsebody <- option Nothing (fmap Just parser)
  statement $ string "endif"
  skipTrailingWhitespace
  return $ Condition cond ifbody elsebody

loopParser :: Parser AST
loopParser = do
  (x, xs) <- statement $ do
               string "for"
               skipSpace
               x <- takeTill (inClass " .[}")
               skipSpace
               string "in"
               skipSpace
               xs <- variableParser
               return (x, xs)
  skipTrailingWhitespace
  loopbody <- parser
  statement $ string "endfor"
  skipTrailingWhitespace
  return $ Foreach x xs loopbody
