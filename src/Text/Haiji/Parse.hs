{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.Haiji.Parse where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Strict
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

data HaijiParserState =
  HaijiParserState
  { haijiParserStateLeadingSpaces :: Maybe AST
  , haijiParserStateInExtends :: Bool
  } deriving (Eq, Show)

defaultHaijiParserState :: HaijiParserState
defaultHaijiParserState =
  HaijiParserState
  { haijiParserStateLeadingSpaces = Nothing
  , haijiParserStateInExtends = False
  }

newtype HaijiParser a =
  HaijiParser
  { unHaijiParser :: StateT HaijiParserState Parser a
  } deriving (Functor, Applicative, Alternative, Monad, MonadState HaijiParserState)

runHaijiParser :: HaijiParser a -> Parser (a, HaijiParserState)
runHaijiParser p = runStateT (unHaijiParser p) defaultHaijiParserState

evalHaijiParser :: HaijiParser a -> Parser a
evalHaijiParser p = fst <$> runHaijiParser p

execHaijiParser :: HaijiParser a -> Parser HaijiParserState
execHaijiParser p = snd <$> runHaijiParser p

liftParser :: Parser a -> HaijiParser a
liftParser = HaijiParser . lift

keepLeadingSpaces :: HaijiParser ()
keepLeadingSpaces = liftParser leadingSpaces >>= setLeadingSpaces where
  leadingSpaces = option Nothing (Just . Literal <$> takeWhile1 isSpace)

setLeadingSpaces :: Maybe AST -> HaijiParser ()
setLeadingSpaces ss = modify (\s -> s { haijiParserStateLeadingSpaces = ss })

resetLeadingSpaces :: HaijiParser ()
resetLeadingSpaces = setLeadingSpaces Nothing

getLeadingSpaces :: HaijiParser (Maybe AST)
getLeadingSpaces = gets haijiParserStateLeadingSpaces

parser :: Parser [AST]
parser = evalHaijiParser (parser' <* liftParser endOfInput)

parser' :: HaijiParser [AST]
parser' = concat <$> many (choice
                           [ toList literalParser
                           , toList derefParser
                           , toList conditionParser
                           , toList foreachParser
                           , toList includeParser
                           , toList rawParser
                           , toList extendsParser
                           , toList blockParser
                           , resetLeadingSpaces *> commentParser *> (maybeToList <$> getLeadingSpaces)
                           ]) where
  toList p = do
    resetLeadingSpaces
    b <- p
    a <- getLeadingSpaces
    return $ maybe id (:) a [b]

-- |
--
-- >>> parseOnly (evalHaijiParser literalParser) "テスト{test"
-- Right テスト
-- >>> parseOnly (evalHaijiParser literalParser) "   テスト  {test"
-- Right    テスト
-- >>> parseOnly (evalHaijiParser literalParser) "   テスト  {%-test"
-- Right    テスト
-- >>> parseOnly (evalHaijiParser literalParser) "   テスト  テスト  {%-test"
-- Right    テスト  テスト
--
literalParser :: HaijiParser AST
literalParser = liftParser $ Literal . T.concat <$> many1 go where
  go = do
    sp <- takeTill (not . isSpace)
    pc <- peekChar
    case pc of
      Nothing  -> if T.null sp then fail "Failed reading: literalParser" else return sp
      Just '{' -> fail "Failed reading: literalParser"
      _        -> T.append sp <$> takeWhile1 (\c -> c /= '{' && not (isSpace c))

-- |
--
-- >>> parseOnly (evalHaijiParser derefParser) "{{ foo }}"
-- Right {{ foo }}
-- >>> parseOnly (execHaijiParser derefParser) "{{ foo }}"
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False})
-- >>> parseOnly (evalHaijiParser derefParser) "{{bar}}"
-- Right {{ bar }}
-- >>> parseOnly (evalHaijiParser derefParser) "{{   baz}}"
-- Right {{ baz }}
-- >>> parseOnly (evalHaijiParser derefParser) " {{ foo }}"
-- Right {{ foo }}
-- >>> parseOnly (execHaijiParser derefParser) " {{ foo }}"
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Just  , haijiParserStateInExtends = False})
-- >>> parseOnly (evalHaijiParser derefParser) "{ { foo }}"
-- Left "Failed reading: takeWith"
-- >>> parseOnly (evalHaijiParser derefParser) "{{ foo } }"
-- Left "Failed reading: takeWith"
-- >>> parseOnly (evalHaijiParser derefParser) "{{ foo }} "
-- Right {{ foo }}
--
derefParser :: HaijiParser AST
derefParser = do
  keepLeadingSpaces
  liftParser $ Deref <$> ((string "{{" >> skipSpace) *> variableParser <* (skipSpace >> string "}}"))

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
-- >>> parseOnly (execHaijiParser $ statement $ return ()) "{%%}"
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False})
-- >>> parseOnly (execHaijiParser $ statement $ return ()) "{% %}"
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False})
-- >>> parseOnly (execHaijiParser $ statement $ return ()) " {% %} "
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Just  , haijiParserStateInExtends = False})
-- >>> parseOnly (execHaijiParser $ statement $ return ()) " {%- -%} "
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False})
--
statement :: Parser a -> HaijiParser a
statement f = withLeadingSpaces <|> skipLeadingSpaces where
  withLeadingSpaces = do
    keepLeadingSpaces
    liftParser $ (string "{%" >> skipSpace) *> f <* (skipSpace >> end)
  skipLeadingSpaces = do
    keepLeadingSpaces
    resetLeadingSpaces
    liftParser $ (string "{%-" >> skipSpace) *> f <* (skipSpace >> end)
  end = string "%}" <|> (string "-%}" <* skipSpace)

-- |
--
-- >>> parseOnly (evalHaijiParser conditionParser) "{% if foo %}テスト{% endif %}"
-- Right {% if foo %}テスト{% endif %}
-- >>> parseOnly (execHaijiParser conditionParser) "{% if foo %}テスト{% endif %}"
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False})
-- >>> parseOnly (evalHaijiParser conditionParser) "{%if foo%}テスト{%endif%}"
-- Right {% if foo %}テスト{% endif %}
-- >>> parseOnly (evalHaijiParser conditionParser) "{% iffoo %}テスト{% endif %}"
-- Left "Failed reading: takeWith"
-- >>> parseOnly (evalHaijiParser conditionParser) "{% if foo %}真{% else %}偽{% endif %}"
-- Right {% if foo %}真{% else %}偽{% endif %}
-- >>> parseOnly (evalHaijiParser conditionParser) "{%if foo%}{%if bar%}{%else%}{%endif%}{%else%}{%if baz%}{%else%}{%endif%}{%endif%}"
-- Right {% if foo %}{% if bar %}{% else %}{% endif %}{% else %}{% if baz %}{% else %}{% endif %}{% endif %}
-- >>> parseOnly (evalHaijiParser conditionParser) "    {% if foo %}テスト{% endif %}"
-- Right {% if foo %}テスト{% endif %}
-- >>> parseOnly (execHaijiParser conditionParser) "    {% if foo %}テスト{% endif %}"
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Just     , haijiParserStateInExtends = False})
-- >>> parseOnly (evalHaijiParser conditionParser) "    {%- if foo -%}    テスト    {%- endif -%}    "
-- Right {% if foo %}テスト{% endif %}
-- >>> parseOnly (execHaijiParser conditionParser) "    {%- if foo -%}    テスト    {%- endif -%}    "
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False})
--
conditionParser :: HaijiParser AST
conditionParser = do
  cond <- statement $ string "if" >> skipMany1 space >> variableParser
  leadingIfSpaces <- getLeadingSpaces
  ifPart <- parser'
  mElsePart <- mayElseParser
  leadingElseSpaces <- getLeadingSpaces
  _ <- statement $ string "endif"
  leadingEndIfSpaces <- getLeadingSpaces
  setLeadingSpaces leadingIfSpaces
  return $ case mElsePart of
    Nothing       -> Condition cond (ifPart ++ maybeToList leadingEndIfSpaces) Nothing
    Just elsePart -> Condition cond (ifPart ++ maybeToList leadingElseSpaces ) (Just $ elsePart ++ maybeToList leadingEndIfSpaces)

mayElseParser :: HaijiParser (Maybe [AST])
mayElseParser = option Nothing (Just <$> elseParser) where
  elseParser = do
    _ <- statement (string "else")
    leadingElseSpaces <- getLeadingSpaces
    elsePart <- parser'
    setLeadingSpaces leadingElseSpaces
    return elsePart

-- |
--
-- >>> parseOnly (evalHaijiParser foreachParser) "{% for _ in foo %}loop{% endfor %}"
-- Right {% for _ in foo %}loop{% endfor %}
-- >>> parseOnly (execHaijiParser foreachParser) "{% for _ in foo %}loop{% endfor %}"
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False})
-- >>> parseOnly (evalHaijiParser foreachParser) "{%for _ in foo%}loop{%endfor%}"
-- Right {% for _ in foo %}loop{% endfor %}
-- >>> parseOnly (evalHaijiParser foreachParser) "{% for_ in foo %}loop{% endfor %}"
-- Left "Failed reading: takeWith"
-- >>> parseOnly (evalHaijiParser foreachParser) "{% for _in foo %}loop{% endfor %}"
-- Left "Failed reading: takeWith"
-- >>> parseOnly (evalHaijiParser foreachParser) "{% for _ infoo %}loop{% endfor %}"
-- Left "Failed reading: takeWith"
-- >>> parseOnly (evalHaijiParser foreachParser) "{% for _ in foo %}loop{% else %}else block{% endfor %}"
-- Right {% for _ in foo %}loop{% else %}else block{% endfor %}
-- >>> parseOnly (evalHaijiParser foreachParser) "{%for _ in foo%}loop{%else%}else block{%endfor%}"
-- Right {% for _ in foo %}loop{% else %}else block{% endfor %}
-- >>> parseOnly (evalHaijiParser foreachParser) "  {% for _ in foo %}  loop  {% endfor %}  "
-- Right {% for _ in foo %}  loop  {% endfor %}
-- >>> parseOnly (execHaijiParser foreachParser) "  {% for _ in foo %}  loop  {% endfor %}  "
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Just   , haijiParserStateInExtends = False})
-- >>> parseOnly (evalHaijiParser foreachParser) "  {%- for _ in foo -%}  loop  {%- endfor -%}  "
-- Right {% for _ in foo %}loop{% endfor %}
-- >>> parseOnly (execHaijiParser foreachParser) "  {%- for _ in foo -%}  loop  {%- endfor -%}  "
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False})
--
foreachParser :: HaijiParser AST
foreachParser = do
  foreach <- statement $ Foreach
             <$> (string "for" >> skipMany1 space >> identifier)
             <*> (skipMany1 space >> string "in" >> skipMany1 space >> variableParser)
  leadingForSpaces <- getLeadingSpaces
  loopPart <- parser'
  mElsePart <- mayElseParser
  leadingElseSpaces <- getLeadingSpaces
  _ <- statement (string "endfor")
  leadingEndForSpaces <- getLeadingSpaces
  setLeadingSpaces leadingForSpaces
  return $ case mElsePart of
    Nothing       -> foreach (loopPart ++ maybeToList leadingEndForSpaces) Nothing
    Just elsePart -> foreach (loopPart ++ maybeToList leadingElseSpaces  ) (Just $ elsePart ++ maybeToList leadingEndForSpaces)

-- |
--
-- >>> parseOnly (evalHaijiParser includeParser) "{% include \"foo.tmpl\" %}"
-- Right {% include "foo.tmpl" %}
-- >>> parseOnly (execHaijiParser includeParser) "{% include \"foo.tmpl\" %}"
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False})
-- >>> parseOnly (evalHaijiParser includeParser) "{%include\"foo.tmpl\"%}"
-- Right {% include "foo.tmpl" %}
-- >>> parseOnly (evalHaijiParser includeParser) "{% include 'foo.tmpl' %}"
-- Right {% include "foo.tmpl" %}
-- >>> parseOnly (evalHaijiParser includeParser) "  {% include \"foo.tmpl\" %}"
-- Right {% include "foo.tmpl" %}
-- >>> parseOnly (execHaijiParser includeParser) "  {% include \"foo.tmpl\" %}"
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Just   , haijiParserStateInExtends = False})
-- >>> parseOnly (evalHaijiParser includeParser) "  {%- include \"foo.tmpl\" -%}   "
-- Right {% include "foo.tmpl" %}
-- >>> parseOnly (execHaijiParser includeParser) "  {%- include \"foo.tmpl\" -%}   "
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False})
--
includeParser :: HaijiParser AST
includeParser = statement $ string "include" >> skipSpace >> Include . T.unpack <$> (quotedBy '"' <|> quotedBy '\'') where
    quotedBy c = char c *> takeTill (== c) <* char c -- TODO: ここもっとマジメにやらないと

-- |
--
-- >>> parseOnly (evalHaijiParser rawParser) "{% raw %}test{% endraw %}"
-- Right {% raw %}test{% endraw %}
-- >>> parseOnly (execHaijiParser rawParser) "{% raw %}test{% endraw %}"
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False})
-- >>> parseOnly (evalHaijiParser rawParser) "{%raw%}test{%endraw%}"
-- Right {% raw %}test{% endraw %}
-- >>> parseOnly (evalHaijiParser rawParser) "{% raw %}{{ test }}{% endraw %}"
-- Right {% raw %}{{ test }}{% endraw %}
-- >>> parseOnly (evalHaijiParser rawParser) "  {% raw %}  test  {% endraw %}"
-- Right {% raw %}  test  {% endraw %}
-- >>> parseOnly (execHaijiParser rawParser) "  {% raw %}  test  {% endraw %}"
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Just   , haijiParserStateInExtends = False})
-- >>> parseOnly (evalHaijiParser rawParser) "  {%- raw -%}   test  {%- endraw -%}  "
-- Right {% raw %}test{% endraw %}
-- >>> parseOnly (execHaijiParser rawParser) "  {%- raw -%}   test  {%- endraw -%}  "
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False})
--
rawParser :: HaijiParser AST
rawParser = do
  _ <- statement (string "raw")
  leadingRawSpaces <- getLeadingSpaces
  (raw, leadingEndRawSpaces) <- till (liftParser anyChar) (statement (string "endraw") >> getLeadingSpaces)
  setLeadingSpaces leadingRawSpaces
  return $ Raw $ raw ++ maybe "" show leadingEndRawSpaces where
    till :: Alternative f => f a -> f b -> f ([a], b)
    till p end = go where
      go = ((,) [] <$> end) <|> ((\a (as,b) -> (a:as, b)) <$> p <*> go)

-- |
--
-- >>> parseOnly (evalHaijiParser extendsParser) "{% extends \"foo.tmpl\" %}"
-- Right {% extends "foo.tmpl" %}
-- >>> parseOnly (execHaijiParser extendsParser) "{% extends \"foo.tmpl\" %}"
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False})
-- >>> parseOnly (evalHaijiParser extendsParser) "{%extends\"foo.tmpl\"%}"
-- Right {% extends "foo.tmpl" %}
-- >>> parseOnly (evalHaijiParser extendsParser) "{% extends 'foo.tmpl' %}"
-- Right {% extends "foo.tmpl" %}
-- >>> parseOnly (evalHaijiParser extendsParser) "  {% extends \"foo.tmpl\" %}"
-- Right {% extends "foo.tmpl" %}
-- >>> parseOnly (execHaijiParser extendsParser) "  {% extends \"foo.tmpl\" %}"
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Just   , haijiParserStateInExtends = False})
-- >>> parseOnly (evalHaijiParser extendsParser) "  {%- extends \"foo.tmpl\" -%}   "
-- Right {% extends "foo.tmpl" %}
-- >>> parseOnly (execHaijiParser extendsParser) "  {%- extends \"foo.tmpl\" -%}   "
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False})
--
extendsParser :: HaijiParser AST
extendsParser = statement $ string "extends" >> skipSpace >> Extends . T.unpack <$> (quotedBy '"' <|> quotedBy '\'') where
  quotedBy c = char c *> takeTill (== c) <* char c -- TODO: ここもっとマジメにやらないと

-- |
--
-- >>> parseOnly (evalHaijiParser blockParser) "{% block foo %}テスト{% endblock %}"
-- Right {% block foo %}テスト{% endblock %}
-- >>> parseOnly (execHaijiParser blockParser) "{% block foo %}テスト{% endblock %}"
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False})
-- >>> parseOnly (evalHaijiParser blockParser) "{% block foo %}テスト{% endblock foo %}"
-- Right {% block foo %}テスト{% endblock %}
-- >>> parseOnly (evalHaijiParser blockParser) "{% block foo %}テスト{% endblock bar %}"
-- Left "Failed reading: blockParser"
-- >>> parseOnly (evalHaijiParser blockParser) "{%block foo%}テスト{%endblock%}"
-- Right {% block foo %}テスト{% endblock %}
-- >>> parseOnly (evalHaijiParser blockParser) "{% blockfoo %}テスト{% endblock %}"
-- Left "Failed reading: takeWith"
-- >>> parseOnly (evalHaijiParser blockParser) "    {% block foo %}テスト{% endblock %}"
-- Right {% block foo %}テスト{% endblock %}
-- >>> parseOnly (execHaijiParser blockParser) "    {% block foo %}テスト{% endblock %}"
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Just     , haijiParserStateInExtends = False})
-- >>> parseOnly (evalHaijiParser blockParser) "    {%- block foo -%}    テスト    {%- endblock -%}    "
-- Right {% block foo %}テスト{% endblock %}
-- >>> parseOnly (execHaijiParser blockParser) "    {%- block foo -%}    テスト    {%- endblock -%}    "
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False})
--
blockParser :: HaijiParser AST
blockParser = do
  name <- statement $ string "block" >> skipMany1 space >> identifier
  leadingBlockSpaces <- getLeadingSpaces
  body <- parser'
  mayEndName <- statement $ string "endblock" >> option Nothing (Just <$> (skipMany1 space >> identifier))
  leadingEndBlockSpaces <- getLeadingSpaces
  setLeadingSpaces leadingBlockSpaces
  if maybe True (name ==) mayEndName
    then return $ Block name False (body ++ maybeToList leadingEndBlockSpaces)
    else fail "blockParser"

-- |
--
-- >>> parseOnly (evalHaijiParser commentParser) "{# comment #}"
-- Right ()
-- >>> parseOnly (execHaijiParser commentParser) "{# comment #}"
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False})
-- >>> parseOnly (evalHaijiParser commentParser) "  {# comment #}"
-- Right ()
-- >>> parseOnly (execHaijiParser commentParser) "  {# comment #}"
-- Right (HaijiParserState {haijiParserStateLeadingSpaces = Just   , haijiParserStateInExtends = False})
--
commentParser :: HaijiParser ()
commentParser = do
  leadingSpaces <- liftParser $ option Nothing (Just . Literal <$> takeWhile1 isSpace)
  setLeadingSpaces leadingSpaces
  _ <- liftParser $ string "{#" >> manyTill anyChar (string "#}")
  return ()
