{-# LANGUAGE OverloadedStrings #-}
module Text.Haiji.Parse where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import qualified Data.Text as T

-- $setup
-- >>> :set -XOverloadedStrings

newtype Identifier = Identifier String deriving Eq

instance Show Identifier where
  show (Identifier x) = x

data Variable = Simple Identifier -- TODO: これだけ別の型にしておきたい
              | Attribute Variable Identifier
              | At Variable Int
                deriving Eq

instance Show Variable where
    show (Simple v) = show v
    show (Attribute v f) = shows v "." ++ show f
    show (At v ix) = shows v "[" ++ show ix ++ "]"

data AST = Literal T.Text
         | Deref Variable
         | Condition Variable [AST] (Maybe [AST])
         | Foreach Identifier Variable [AST] (Maybe [AST])
         | Include FilePath
         | Raw String
           deriving Eq

instance Show AST where
    show (Literal l) = T.unpack l
    show (Deref v) = "{{ " ++ shows v " }}"
    show (Condition p ts mfs) =
      "{% if " ++ show p ++ " %}" ++
      concatMap show ts ++
      maybe "" (\fs -> "{% else %}" ++ concatMap show fs) mfs ++
      "{% endif %}"
    show (Foreach x xs loopBody elseBody) =
      "{% for " ++ show x ++ " in " ++ show xs ++ " %}" ++
      concatMap show loopBody ++
      maybe "" (("{% else %}" ++) . concatMap show) elseBody ++
      "{% endfor %}"
    show (Include file) = "{% include \"" ++ file ++ "\" %}"
    show (Raw raw) = "{% raw %}" ++ raw ++ "{% endraw %}"

parser :: Parser [AST]
parser = parser' <* endOfInput

parser' :: Parser [AST]
parser' = many $ choice [ literalParser
                        , derefParser
                        , conditionParser
                        , foreachParser
                        , includeParser
                        , rawParser
                        ]

-- |
--
-- >>> parseOnly literalParser "テスト{test"
-- Right テスト
--
literalParser :: Parser AST
literalParser = Literal <$> takeWhile1 (/= '{')

-- |
--
-- >>> parseOnly derefParser "{{ foo }}"
-- Right {{ foo }}
-- >>> parseOnly derefParser "{{bar}}"
-- Right {{ bar }}
-- >>> parseOnly derefParser "{{   baz}}"
-- Right {{ baz }}
-- >>> parseOnly derefParser " {{ foo }}"
-- Left "Failed reading: takeWith"
-- >>> parseOnly derefParser "{ { foo }}"
-- Left "Failed reading: takeWith"
-- >>> parseOnly derefParser "{{ foo } }"
-- Left "Failed reading: takeWith"
-- >>> parseOnly derefParser "{{ foo }} "
-- Right {{ foo }}
--
derefParser :: Parser AST
derefParser = Deref <$> ((string "{{" >> skipSpace) *> variableParser <* (skipSpace >> string "}}"))

-- | python identifier
--
-- https://docs.python.org/2.7/reference/lexical_analysis.html#identifiers
--
-- >>> parseOnly identifier "a"
-- Right a
-- >>> parseOnly identifier "ab"
-- Right ab
-- >>> parseOnly identifier "A"
-- Right A
-- >>> parseOnly identifier "Ab"
-- Right Ab
-- >>> parseOnly identifier "_"
-- Right _
-- >>> parseOnly identifier "_a"
-- Right _a
-- >>> parseOnly identifier "_1"
-- Right _1
-- >>> parseOnly identifier "__"
-- Right __
-- >>> parseOnly identifier "_ "
-- Right _
-- >>> parseOnly identifier " _"
-- Left "Failed reading: satisfy"
-- >>> parseOnly identifier "and"
-- Left "Failed reading: identifier"
-- >>> parseOnly identifier "1"
-- Left "Failed reading: satisfy"
-- >>> parseOnly identifier "1b"
-- Left "Failed reading: satisfy"
-- >>> parseOnly identifier "'x"
-- Left "Failed reading: satisfy"
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
keywords = words
           $  "and       del       from      not       while "
           ++ "as        elif      global    or        with  "
           ++ "assert    else      if        pass      yield "
           ++ "break     except    import    print           "
           ++ "class     exec      in        raise           "
           ++ "continue  finally   is        return          "
           ++ "def       for       lambda    try             "

-- |
--
-- >>> parseOnly variableParser "foo"
-- Right foo
-- >>> parseOnly variableParser "foo.bar"
-- Right foo.bar
-- >>> parseOnly variableParser "foo[0]"
-- Right foo[0]
-- >>> parseOnly variableParser "foo.bar[0]"
-- Right foo.bar[0]
-- >>> parseOnly variableParser "foo[0].bar"
-- Right foo[0].bar
-- >>> parseOnly variableParser "foo.b}}ar"
-- Right foo.b
-- >>> parseOnly variableParser "foo.b ar"
-- Right foo.b
-- >>> parseOnly variableParser "foo.b }ar"
-- Right foo.b
-- >>> parseOnly variableParser " foo.bar"
-- Left "Failed reading: satisfy"
-- >>> parseOnly variableParser "foo.  bar"
-- Right foo.bar
-- >>> parseOnly variableParser "foo  .bar"
-- Right foo.bar
-- >>> parseOnly variableParser "foo.bar  "
-- Right foo.bar
-- >>> parseOnly variableParser "foo.bar  "
-- Right foo.bar
-- >>> parseOnly variableParser "foo  [0]"
-- Right foo[0]
-- >>> parseOnly variableParser "foo  [  0  ]"
-- Right foo[0]
--
variableParser :: Parser Variable
variableParser = identifier >>= variableParser' . Simple where
    variableParser' v = do
      skipSpace
      peek <- peekChar
      case peek of
        Nothing  -> return v
        Just '}' -> return v
        Just ' ' -> return v
        Just '.' -> char '.' >> skipSpace >> identifier >>= variableParser' . Attribute v
        Just '[' -> (char '[' >> skipSpace) *> decimal <* (skipSpace >> char ']') >>= variableParser' . At v
        _        -> return v

statement :: Parser a -> Parser a
statement f = (string "{%" >> skipSpace) *> f <* (skipSpace >> string "%}")

-- |
--
-- >>> parseOnly conditionParser "{% if foo %}テスト{% endif %}"
-- Right {% if foo %}テスト{% endif %}
-- >>> parseOnly conditionParser "{% if foo %}真{% else %}偽{% endif %}"
-- Right {% if foo %}真{% else %}偽{% endif %}
-- >>> parseOnly conditionParser "{%if foo%}{%if bar%}{%else%}{%endif%}{%else%}{%if baz%}{%else%}{%endif%}{%endif%}"
-- Right {% if foo %}{% if bar %}{% else %}{% endif %}{% else %}{% if baz %}{% else %}{% endif %}{% endif %}
--
conditionParser :: Parser AST
conditionParser = do
  cond <- statement $ string "if" >> skipSpace >> variableParser
  ifbody <- parser'
  elsebody <- option Nothing (Just <$> (statement (string "else") *> parser'))
  _ <- statement $ string "endif"
  return $ Condition cond ifbody elsebody

-- |
--
-- >>> parseOnly foreachParser "{% for _ in foo %}loop{% endfor %}"
-- Right {% for _ in foo %}loop{% endfor %}
-- >>> parseOnly foreachParser "{% for _ in foo %}loop{% else %}else block{% endfor %}"
-- Right {% for _ in foo %}loop{% else %}else block{% endfor %}
--
foreachParser :: Parser AST
foreachParser = do
  foreach <- statement $ Foreach
                  <$> (string "for" >> skipSpace >> identifier)
                  <*> (skipSpace >> string "in" >> skipSpace >> variableParser)
  loopBlock <- parser'
  elseBlock <- option Nothing (Just <$> (statement (string "else") *> parser'))
  _ <- statement (string "endfor")
  foreach <$> return loopBlock  <*> return elseBlock

-- |
--
-- >>> parseOnly includeParser "{% include \"foo.tmpl\" %}"
-- Right {% include "foo.tmpl" %}
-- >>> parseOnly includeParser "{% include 'foo.tmpl' %}"
-- Right {% include "foo.tmpl" %}
--
includeParser :: Parser AST
includeParser = statement $ string "include" >> skipSpace >> Include . T.unpack <$> (quotedBy '"' <|> quotedBy '\'') where
    quotedBy c = char c *> takeTill (== c) <* char c -- TODO: ここもっとマジメにやらないと

-- |
--
-- >>> parseOnly rawParser "{% raw %}test{% endraw %}"
-- Right {% raw %}test{% endraw %}
-- >>> parseOnly rawParser "{% raw %}{{ test }}{% endraw %}"
-- Right {% raw %}{{ test }}{% endraw %}
--
rawParser :: Parser AST
rawParser = Raw <$> (statement (string "raw") *> manyTill anyChar (statement $ string "endraw"))
