{-# LANGUAGE OverloadedStrings #-}
module Text.Haiji.Parse where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.Char
import Data.Maybe
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
         | Extends FilePath
         | Block Identifier Bool [AST]
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
  show (Extends file) = "{% extends \"" ++ file ++ "\" %}"
  show (Block name scoped body) =
    "{% block " ++ show name ++ (if scoped then " scoped" else "") ++" %}" ++
    concatMap show body ++
    "{% endblock %}"

parser :: Parser [AST]
parser = parser' <* endOfInput

parser' :: Parser [AST]
parser' = concat <$> many (choice
                           [ pure <$> literalParser
                           , toList <$> derefParser
                           , toList <$> conditionParser
                           , toList <$> foreachParser
                           , toList <$> includeParser
                           , toList <$> rawParser
                           , toList <$> extendsParser
                           , toList <$> blockParser
                           , maybeToList <$> commentParser
                           ]) where
  toList (a, b) = maybe id (:) a [b]

-- |
--
-- >>> parseOnly literalParser "テスト{test"
-- Right テスト
-- >>> parseOnly literalParser "   テスト  {test"
-- Right    テスト
-- >>> parseOnly literalParser "   テスト  {%-test"
-- Right    テスト
-- >>> parseOnly literalParser "   テスト  テスト  {%-test"
-- Right    テスト  テスト
--
literalParser :: Parser AST
literalParser = Literal . T.concat <$> many1 go where
  go = do
    sp <- takeTill (not . isSpace)
    pc <- peekChar
    case pc of
      Nothing  -> if T.null sp then fail "Failed reading: literalParser" else return sp
      Just '{' -> fail "Failed reading: literalParser"
      _        -> T.append sp <$> takeWhile1 (\c -> c /= '{' && not (isSpace c))

-- |
--
-- >>> parseOnly derefParser "{{ foo }}"
-- Right (Nothing,{{ foo }})
-- >>> parseOnly derefParser "{{bar}}"
-- Right (Nothing,{{ bar }})
-- >>> parseOnly derefParser "{{   baz}}"
-- Right (Nothing,{{ baz }})
-- >>> parseOnly derefParser " {{ foo }}"
-- Right (Just  ,{{ foo }})
-- >>> parseOnly derefParser "{ { foo }}"
-- Left "Failed reading: takeWith"
-- >>> parseOnly derefParser "{{ foo } }"
-- Left "Failed reading: takeWith"
-- >>> parseOnly derefParser "{{ foo }} "
-- Right (Nothing,{{ foo }})
--
derefParser :: Parser (Maybe AST, AST)
derefParser = do
  preSpaces <- option Nothing (Just . Literal <$> takeWhile1 isSpace)
  (,) preSpaces . Deref <$> ((string "{{" >> skipSpace) *> variableParser <* (skipSpace >> string "}}"))

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

-- |
--
-- >>> parseOnly (statement $ return ()) "{%%}"
-- Right (Nothing,())
-- >>> parseOnly (statement $ return ()) "{% %}"
-- Right (Nothing,())
-- >>> parseOnly (statement $ return ()) " {% %} "
-- Right (Just  ,())
-- >>> parseOnly (statement $ return ()) " {%- -%} "
-- Right (Nothing,())
--
statement :: Parser a -> Parser (Maybe AST, a)
statement f = do
  preSpaces <- option Nothing (Just . Literal <$> takeWhile1 isSpace)
  ((string "{%" >> skipSpace) *> ((,) preSpaces <$> f) <* (skipSpace >> end)) <|> ((string "{%-" >> skipSpace) *> ((,) Nothing <$> f) <* (skipSpace >> end)) where
    end = string "%}" <|> (string "-%}" <* skipSpace)

-- |
--
-- >>> parseOnly conditionParser "{% if foo %}テスト{% endif %}"
-- Right (Nothing,{% if foo %}テスト{% endif %})
-- >>> parseOnly conditionParser "{%if foo%}テスト{%endif%}"
-- Right (Nothing,{% if foo %}テスト{% endif %})
-- >>> parseOnly conditionParser "{% iffoo %}テスト{% endif %}"
-- Left "Failed reading: takeWith"
-- >>> parseOnly conditionParser "{% if foo %}真{% else %}偽{% endif %}"
-- Right (Nothing,{% if foo %}真{% else %}偽{% endif %})
-- >>> parseOnly conditionParser "{%if foo%}{%if bar%}{%else%}{%endif%}{%else%}{%if baz%}{%else%}{%endif%}{%endif%}"
-- Right (Nothing,{% if foo %}{% if bar %}{% else %}{% endif %}{% else %}{% if baz %}{% else %}{% endif %}{% endif %})
-- >>> parseOnly conditionParser "    {% if foo %}テスト{% endif %}"
-- Right (Just     ,{% if foo %}テスト{% endif %})
-- >>> parseOnly conditionParser "    {%- if foo -%}    テスト    {%- endif -%}    "
-- Right (Nothing,{% if foo %}テスト{% endif %})
--
conditionParser :: Parser (Maybe AST, AST)
conditionParser = do
  (preIfSpaces, cond) <- statement $ string "if" >> skipMany1 space >> variableParser
  ifPart <- parser'
  mElsePart <- option Nothing (do (preElseSpaces, _) <- statement (string "else")
                                  Just . (,) preElseSpaces <$> parser')
  (preEndIfSpaces, _) <- statement $ string "endif"
  return (preIfSpaces,
          case mElsePart of
            Nothing                        -> Condition cond (ifPart ++ maybeToList preEndIfSpaces) Nothing
            Just (preElseSpaces, elsePart) -> Condition cond (ifPart ++ maybeToList preElseSpaces ) (Just $ elsePart ++ maybeToList preEndIfSpaces)
         )

-- |
--
-- >>> parseOnly foreachParser "{% for _ in foo %}loop{% endfor %}"
-- Right (Nothing,{% for _ in foo %}loop{% endfor %})
-- >>> parseOnly foreachParser "{%for _ in foo%}loop{%endfor%}"
-- Right (Nothing,{% for _ in foo %}loop{% endfor %})
-- >>> parseOnly foreachParser "{% for_ in foo %}loop{% endfor %}"
-- Left "Failed reading: takeWith"
-- >>> parseOnly foreachParser "{% for _in foo %}loop{% endfor %}"
-- Left "Failed reading: takeWith"
-- >>> parseOnly foreachParser "{% for _ infoo %}loop{% endfor %}"
-- Left "Failed reading: takeWith"
-- >>> parseOnly foreachParser "{% for _ in foo %}loop{% else %}else block{% endfor %}"
-- Right (Nothing,{% for _ in foo %}loop{% else %}else block{% endfor %})
-- >>> parseOnly foreachParser "{%for _ in foo%}loop{%else%}else block{%endfor%}"
-- Right (Nothing,{% for _ in foo %}loop{% else %}else block{% endfor %})
-- >>> parseOnly foreachParser "  {% for _ in foo %}  loop  {% endfor %}  "
-- Right (Just   ,{% for _ in foo %}  loop  {% endfor %})
-- >>> parseOnly foreachParser "  {%- for _ in foo -%}  loop  {%- endfor -%}  "
-- Right (Nothing,{% for _ in foo %}loop{% endfor %})
--
foreachParser :: Parser (Maybe AST, AST)
foreachParser = do
  (preForSpaces, foreach) <- statement $ Foreach
                             <$> (string "for" >> skipMany1 space >> identifier)
                             <*> (skipMany1 space >> string "in" >> skipMany1 space >> variableParser)
  loopPart <- parser'
  mElsePart <- option Nothing (do (preElseSpaces, _) <- statement (string "else")
                                  Just . (,) preElseSpaces <$> parser')
  (preEndForSpaces, _) <- statement (string "endfor")
  (,) preForSpaces <$> case mElsePart of
    Nothing                        -> foreach <$> return (loopPart ++ maybeToList preEndForSpaces) <*> return Nothing
    Just (preElseSpaces, elsePart) -> foreach <$> return (loopPart ++ maybeToList preElseSpaces  ) <*> return (Just $ elsePart ++ maybeToList preEndForSpaces)

-- |
--
-- >>> parseOnly includeParser "{% include \"foo.tmpl\" %}"
-- Right (Nothing,{% include "foo.tmpl" %})
-- >>> parseOnly includeParser "{%include\"foo.tmpl\"%}"
-- Right (Nothing,{% include "foo.tmpl" %})
-- >>> parseOnly includeParser "{% include 'foo.tmpl' %}"
-- Right (Nothing,{% include "foo.tmpl" %})
-- >>> parseOnly includeParser "  {% include \"foo.tmpl\" %}"
-- Right (Just   ,{% include "foo.tmpl" %})
-- >>> parseOnly includeParser "  {%- include \"foo.tmpl\" -%}   "
-- Right (Nothing,{% include "foo.tmpl" %})
--
includeParser :: Parser (Maybe AST, AST)
includeParser = statement $ string "include" >> skipSpace >> Include . T.unpack <$> (quotedBy '"' <|> quotedBy '\'') where
    quotedBy c = char c *> takeTill (== c) <* char c -- TODO: ここもっとマジメにやらないと

-- |
--
-- >>> parseOnly rawParser "{% raw %}test{% endraw %}"
-- Right (Nothing,{% raw %}test{% endraw %})
-- >>> parseOnly rawParser "{%raw%}test{%endraw%}"
-- Right (Nothing,{% raw %}test{% endraw %})
-- >>> parseOnly rawParser "{% raw %}{{ test }}{% endraw %}"
-- Right (Nothing,{% raw %}{{ test }}{% endraw %})
-- >>> parseOnly rawParser "  {% raw %}  test  {% endraw %}"
-- Right (Just   ,{% raw %}  test  {% endraw %})
-- >>> parseOnly rawParser "  {%- raw -%}   test  {%- endraw -%}  "
-- Right (Nothing,{% raw %}test{% endraw %})
--
rawParser :: Parser (Maybe AST, AST)
rawParser = do
  (preRawSpaces, _) <- statement (string "raw")
  (raw, (preEndRawSpaces, _)) <- till anyChar (statement $ string "endraw")
  return (preRawSpaces, Raw $ raw ++ maybe "" show preEndRawSpaces) where
    till :: Alternative f => f a -> f b -> f ([a], b)
    till p end = go where
      go = ((,) [] <$> end) <|> ((\a (as,b) -> (a:as, b)) <$> p <*> go)

-- |
--
-- >>> parseOnly extendsParser "{% extends \"foo.tmpl\" %}"
-- Right (Nothing,{% extends "foo.tmpl" %})
-- >>> parseOnly extendsParser "{%extends\"foo.tmpl\"%}"
-- Right (Nothing,{% extends "foo.tmpl" %})
-- >>> parseOnly extendsParser "{% extends 'foo.tmpl' %}"
-- Right (Nothing,{% extends "foo.tmpl" %})
-- >>> parseOnly extendsParser "  {% extends \"foo.tmpl\" %}"
-- Right (Just   ,{% extends "foo.tmpl" %})
-- >>> parseOnly extendsParser "  {%- extends \"foo.tmpl\" -%}   "
-- Right (Nothing,{% extends "foo.tmpl" %})
--
extendsParser :: Parser (Maybe AST, AST)
extendsParser = statement $ string "extends" >> skipSpace >> Extends . T.unpack <$> (quotedBy '"' <|> quotedBy '\'') where
  quotedBy c = char c *> takeTill (== c) <* char c -- TODO: ここもっとマジメにやらないと

-- |
--
-- >>> parseOnly blockParser "{% block foo %}テスト{% endblock %}"
-- Right (Nothing,{% block foo %}テスト{% endblock %})
-- >>> parseOnly blockParser "{% block foo %}テスト{% endblock foo %}"
-- Right (Nothing,{% block foo %}テスト{% endblock %})
-- >>> parseOnly blockParser "{% block foo %}テスト{% endblock bar %}"
-- Left "Failed reading: blockParser"
-- >>> parseOnly blockParser "{%block foo%}テスト{%endblock%}"
-- Right (Nothing,{% block foo %}テスト{% endblock %})
-- >>> parseOnly blockParser "{% blockfoo %}テスト{% endblock %}"
-- Left "Failed reading: takeWith"
-- >>> parseOnly blockParser "    {% block foo %}テスト{% endblock %}"
-- Right (Just     ,{% block foo %}テスト{% endblock %})
-- >>> parseOnly blockParser "    {%- block foo -%}    テスト    {%- endblock -%}    "
-- Right (Nothing,{% block foo %}テスト{% endblock %})
-- >>> parseOnly blockParser "    {%- block foo -%}    テスト    {%- endblock -%}    "
-- Right (Nothing,{% block foo %}テスト{% endblock %})
--
blockParser :: Parser (Maybe AST, AST)
blockParser = do
  (preBlockSpaces, name) <- statement $ string "block" >> skipMany1 space >> identifier
  body <- parser'
  (preEndBlockSpaces, mayEndName) <- statement $ string "endblock" >> option Nothing (Just <$> (skipMany1 space >> identifier))
  if maybe True (name ==) mayEndName
    then return (preBlockSpaces, Block name False (body ++ maybeToList preEndBlockSpaces))
    else fail "blockParser"

-- |
--
-- >>> parseOnly commentParser "{# comment #}"
-- Right Nothing
-- >>> parseOnly commentParser "  {# comment #}"
-- Right (Just   )
--
commentParser :: Parser (Maybe AST)
commentParser = do
  preSpaces <- option Nothing (Just . Literal <$> takeWhile1 isSpace)
  _ <- string "{#" >> manyTill anyChar (string "#}")
  return preSpaces
