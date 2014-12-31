{-# LANGUAGE OverloadedStrings #-}
module Text.Haiji.Parse where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Text as T

data Variable = Simple T.Text
              | Attribute Variable T.Text
              | At Variable Int
                deriving Eq

instance Show Variable where
    show (Simple v) = T.unpack v
    show (Attribute v f) = shows v "." ++ T.unpack f
    show (At v ix) = shows v "[" ++ show ix ++ "]"

data AST = Literal T.Text
         | Deref Variable
         | Condition Variable [AST] (Maybe [AST])
         | Foreach T.Text Variable [AST]
         | Include FilePath
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
    show (Include file) = "{% include \"" ++ file ++ "\" %}"

parser :: Parser [AST]
parser = many $ choice [ literalParser
                       , derefParser
                       , conditionParser
                       , foreachParser
                       , includeParser
                       ]

literalParser :: Parser AST
literalParser = Literal <$> takeWhile1 (/= '{')

derefParser :: Parser AST
derefParser = Deref <$> ((string "{{" >> skipSpace) *> variableParser <* (skipSpace >> string "}}"))

variableParser :: Parser Variable
variableParser = ident >>= variableParser' . Simple where
    ident = takeTill (inClass " .[}")
    variableParser' v = do
      peek <- peekChar
      case peek of
        Nothing  -> return v
        Just '}' -> return v
        Just ' ' -> return v
        Just '.' -> char '.' >> ident >>= variableParser' . Attribute v
        Just '[' -> (char '[' >> skipSpace) *> decimal <* (skipSpace >> char ']') >>= variableParser' . At v
        _        -> fail "variableParser: invalid variable"

statement :: Parser a -> Parser a
statement f = (string "{%" >> skipSpace) *> f <* (skipSpace >> string "%}")

conditionParser :: Parser AST
conditionParser = do
  cond <- statement $ string "if" >> skipSpace >> variableParser
  ifbody <- parser
  _ <- statement $ string "endif" <|> string "else" -- このあたり間違ってる
  elsebody <- option Nothing (Just <$> parser)      --
  _ <- statement $ string "endif"
  return $ Condition cond ifbody elsebody

foreachParser :: Parser AST
foreachParser = do
  foreachBlock <- statement $ Foreach
                  <$> (string "for" >> skipSpace >> takeTill (inClass " .[}"))
                  <*> (skipSpace >> string "in" >> skipSpace >> variableParser)
  foreachBlock <$> parser <* statement (string "endfor")

includeParser :: Parser AST
includeParser = statement $ string "include" >> skipSpace >> Include . T.unpack <$> (quotedBy '"' <|> quotedBy '\'') where
    quotedBy c = char c *> takeTill (== c) <* char c
