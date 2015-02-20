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

saveLeadingSpaces :: HaijiParser ()
saveLeadingSpaces = liftParser leadingSpaces >>= setLeadingSpaces where
  leadingSpaces = option Nothing (Just . Literal <$> takeWhile1 isSpace)

withLeadingSpacesOf :: HaijiParser a -> (a -> HaijiParser b) -> HaijiParser b
withLeadingSpacesOf p q = do
  a <- p
  getLeadingSpaces >>= (q a <*) . setLeadingSpaces

setLeadingSpaces :: Maybe AST -> HaijiParser ()
setLeadingSpaces ss = modify (\s -> s { haijiParserStateLeadingSpaces = ss })

resetLeadingSpaces :: HaijiParser ()
resetLeadingSpaces = setLeadingSpaces Nothing

getLeadingSpaces :: HaijiParser (Maybe AST)
getLeadingSpaces = gets haijiParserStateLeadingSpaces

parser :: Parser [AST]
parser = evalHaijiParser (haijiParser <* liftParser endOfInput)

haijiParser :: HaijiParser [AST]
haijiParser = concat <$> many (resetLeadingSpaces *>
                               choice
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
    b <- p
    a <- getLeadingSpaces
    return $ maybe id (:) a [b]

-- |
--
-- >>> let eval = either error id . parseOnly (evalHaijiParser literalParser)
-- >>> eval "テスト{test"
-- テスト
-- >>> eval "   テスト  {test"
--    テスト
-- >>> eval "   テスト  {%-test"
--    テスト
-- >>> eval "   テスト  テスト  {%-test"
--    テスト  テスト
--
literalParser :: HaijiParser AST
literalParser = liftParser $ Literal . T.concat <$> many1 go where
  go = do
    sp <- takeTill (not . isSpace)
    pc <- peekChar
    case pc of
      Nothing  -> if T.null sp then fail "literalParser" else return sp
      Just '{' -> fail "literalParser"
      _        -> T.append sp <$> takeWhile1 (\c -> c /= '{' && not (isSpace c))

-- |
--
-- >>> let eval = either error id . parseOnly (evalHaijiParser derefParser)
-- >>> let exec = either error id . parseOnly (execHaijiParser derefParser)
-- >>> eval "{{ foo }}"
-- {{ foo }}
-- >>> exec "{{ foo }}"
-- HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False}
-- >>> eval "{{bar}}"
-- {{ bar }}
-- >>> eval "{{   baz}}"
-- {{ baz }}
-- >>> eval " {{ foo }}"
-- {{ foo }}
-- >>> exec " {{ foo }}"
-- HaijiParserState {haijiParserStateLeadingSpaces = Just  , haijiParserStateInExtends = False}
-- >>> eval "{ { foo }}"
-- *** Exception: Failed reading: takeWith
-- >>> eval "{{ foo } }"
-- *** Exception: Failed reading: takeWith
-- >>> eval "{{ foo }} "
-- {{ foo }}
--
derefParser :: HaijiParser AST
derefParser = saveLeadingSpaces *> liftParser deref where
  deref = Deref <$> ((string "{{" >> skipSpace) *> variableParser <* (skipSpace >> string "}}"))

-- | python identifier
--
-- https://docs.python.org/2.7/reference/lexical_analysis.html#identifiers
--
-- >>> let eval = either error id . parseOnly identifier
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
-- *** Exception: Failed reading: satisfy
-- >>> eval "and"
-- *** Exception: Failed reading: identifier
-- >>> eval "1"
-- *** Exception: Failed reading: satisfy
-- >>> eval "1b"
-- *** Exception: Failed reading: satisfy
-- >>> eval "'x"
-- *** Exception: Failed reading: satisfy
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
-- >>> let eval = either error id . parseOnly variableParser
-- >>> eval "foo"
-- foo
-- >>> eval "foo.bar"
-- foo.bar
-- >>> eval "foo[0]"
-- foo[0]
-- >>> eval "foo.bar[0]"
-- foo.bar[0]
-- >>> eval "foo[0].bar"
-- foo[0].bar
-- >>> eval "foo.b}}ar"
-- foo.b
-- >>> eval "foo.b ar"
-- foo.b
-- >>> eval "foo.b }ar"
-- foo.b
-- >>> eval " foo.bar"
-- *** Exception: Failed reading: satisfy
-- >>> eval "foo.  bar"
-- foo.bar
-- >>> eval "foo  .bar"
-- foo.bar
-- >>> eval "foo.bar  "
-- foo.bar
-- >>> eval "foo.bar  "
-- foo.bar
-- >>> eval "foo  [0]"
-- foo[0]
-- >>> eval "foo  [  0  ]"
-- foo[0]
--
variableParser :: Parser Variable
variableParser = identifier >>= go . Simple where
  go v = do
    skipSpace
    peek <- peekChar
    case peek of
      Nothing  -> return v
      Just '}' -> return v
      Just ' ' -> return v
      Just '.' -> char '.' >> skipSpace >> identifier >>= go . Attribute v
      Just '[' -> (char '[' >> skipSpace) *> decimal <* (skipSpace >> char ']') >>= go . At v
      _        -> return v

-- |
--
-- >>> let exec = either error id . parseOnly (execHaijiParser $ statement $ return ())
-- >>> exec "{%%}"
-- HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False}
-- >>> exec "{% %}"
-- HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False}
-- >>> exec " {% %} "
-- HaijiParserState {haijiParserStateLeadingSpaces = Just  , haijiParserStateInExtends = False}
-- >>> exec " {%- -%} "
-- HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False}
--
statement :: Parser a -> HaijiParser a
statement f = start "{%" <|> (start "{%-" <* resetLeadingSpaces) where
  start s = saveLeadingSpaces *> liftParser ((string s  >> skipSpace) *> f <* (skipSpace >> end))
  end = string "%}" <|> (string "-%}" <* skipSpace)

-- |
--
-- >>> let eval = either error id . parseOnly (evalHaijiParser conditionParser)
-- >>> let exec = either error id . parseOnly (execHaijiParser conditionParser)
-- >>> eval "{% if foo %}テスト{% endif %}"
-- {% if foo %}テスト{% endif %}
-- >>> exec "{% if foo %}テスト{% endif %}"
-- HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False}
-- >>> eval "{%if foo%}テスト{%endif%}"
-- {% if foo %}テスト{% endif %}
-- >>> eval "{% iffoo %}テスト{% endif %}"
-- *** Exception: Failed reading: takeWith
-- >>> eval "{% if foo %}真{% else %}偽{% endif %}"
-- {% if foo %}真{% else %}偽{% endif %}
-- >>> eval "{%if foo%}{%if bar%}{%else%}{%endif%}{%else%}{%if baz%}{%else%}{%endif%}{%endif%}"
-- {% if foo %}{% if bar %}{% else %}{% endif %}{% else %}{% if baz %}{% else %}{% endif %}{% endif %}
-- >>> eval "    {% if foo %}テスト{% endif %}"
-- {% if foo %}テスト{% endif %}
-- >>> exec "    {% if foo %}テスト{% endif %}"
-- HaijiParserState {haijiParserStateLeadingSpaces = Just     , haijiParserStateInExtends = False}
-- >>> eval "    {%- if foo -%}    テスト    {%- endif -%}    "
-- {% if foo %}テスト{% endif %}
-- >>> exec "    {%- if foo -%}    テスト    {%- endif -%}    "
-- HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False}
--
conditionParser :: HaijiParser AST
conditionParser = withLeadingSpacesOf startCondition restCondition where
  startCondition = statement $ string "if" >> skipMany1 space >> variableParser
  restCondition cond = do
    ifPart <- haijiParser
    mElsePart <- mayElseParser
    leadingElseSpaces <- getLeadingSpaces
    _ <- statement $ string "endif"
    leadingEndIfSpaces <- getLeadingSpaces
    return $ case mElsePart of
      Nothing       -> Condition cond (ifPart ++ maybeToList leadingEndIfSpaces) Nothing
      Just elsePart -> Condition cond (ifPart ++ maybeToList leadingElseSpaces ) (Just $ elsePart ++ maybeToList leadingEndIfSpaces)

mayElseParser :: HaijiParser (Maybe [AST])
mayElseParser = option Nothing (Just <$> elseParser) where
  elseParser = withLeadingSpacesOf (statement (string "else")) $ const haijiParser

-- |
--
-- >>> let eval = either error id . parseOnly (evalHaijiParser foreachParser)
-- >>> let exec = either error id . parseOnly (execHaijiParser foreachParser)
-- >>> eval "{% for _ in foo %}loop{% endfor %}"
-- {% for _ in foo %}loop{% endfor %}
-- >>> exec "{% for _ in foo %}loop{% endfor %}"
-- HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False}
-- >>> eval "{%for _ in foo%}loop{%endfor%}"
-- {% for _ in foo %}loop{% endfor %}
-- >>> eval "{% for_ in foo %}loop{% endfor %}"
-- *** Exception: Failed reading: takeWith
-- >>> eval "{% for _in foo %}loop{% endfor %}"
-- *** Exception: Failed reading: takeWith
-- >>> eval "{% for _ infoo %}loop{% endfor %}"
-- *** Exception: Failed reading: takeWith
-- >>> eval "{% for _ in foo %}loop{% else %}else block{% endfor %}"
-- {% for _ in foo %}loop{% else %}else block{% endfor %}
-- >>> eval "{%for _ in foo%}loop{%else%}else block{%endfor%}"
-- {% for _ in foo %}loop{% else %}else block{% endfor %}
-- >>> eval "  {% for _ in foo %}  loop  {% endfor %}  "
-- {% for _ in foo %}  loop  {% endfor %}
-- >>> exec "  {% for _ in foo %}  loop  {% endfor %}  "
-- HaijiParserState {haijiParserStateLeadingSpaces = Just   , haijiParserStateInExtends = False}
-- >>> eval "  {%- for _ in foo -%}  loop  {%- endfor -%}  "
-- {% for _ in foo %}loop{% endfor %}
-- >>> exec "  {%- for _ in foo -%}  loop  {%- endfor -%}  "
-- HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False}
--
foreachParser :: HaijiParser AST
foreachParser = withLeadingSpacesOf startForeach restForeach where
  startForeach = statement $ Foreach
                 <$> (string "for" >> skipMany1 space >> identifier)
                 <*> (skipMany1 space >> string "in" >> skipMany1 space >> variableParser)
  restForeach foreach = do
    loopPart <- haijiParser
    mElsePart <- mayElseParser
    leadingElseSpaces <- getLeadingSpaces
    _ <- statement (string "endfor")
    leadingEndForSpaces <- getLeadingSpaces
    return $ case mElsePart of
      Nothing       -> foreach (loopPart ++ maybeToList leadingEndForSpaces) Nothing
      Just elsePart -> foreach (loopPart ++ maybeToList leadingElseSpaces  ) (Just $ elsePart ++ maybeToList leadingEndForSpaces)

-- |
--
-- >>> let eval = either error id . parseOnly (evalHaijiParser includeParser)
-- >>> let exec = either error id . parseOnly (execHaijiParser includeParser)
-- >>> eval "{% include \"foo.tmpl\" %}"
-- {% include "foo.tmpl" %}
-- >>> exec "{% include \"foo.tmpl\" %}"
-- HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False}
-- >>> eval "{%include\"foo.tmpl\"%}"
-- {% include "foo.tmpl" %}
-- >>> eval "{% include 'foo.tmpl' %}"
-- {% include "foo.tmpl" %}
-- >>> eval "  {% include \"foo.tmpl\" %}"
-- {% include "foo.tmpl" %}
-- >>> exec "  {% include \"foo.tmpl\" %}"
-- HaijiParserState {haijiParserStateLeadingSpaces = Just   , haijiParserStateInExtends = False}
-- >>> eval "  {%- include \"foo.tmpl\" -%}   "
-- {% include "foo.tmpl" %}
-- >>> exec "  {%- include \"foo.tmpl\" -%}   "
-- HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False}
--
includeParser :: HaijiParser AST
includeParser = statement $ string "include" >> skipSpace >> Include . T.unpack <$> (quotedBy '"' <|> quotedBy '\'') where
    quotedBy c = char c *> takeTill (== c) <* char c -- TODO: ここもっとマジメにやらないと

-- |
--
-- >>> let eval = either error id . parseOnly (evalHaijiParser rawParser)
-- >>> let exec = either error id . parseOnly (execHaijiParser rawParser)
-- >>> eval "{% raw %}test{% endraw %}"
-- {% raw %}test{% endraw %}
-- >>> exec "{% raw %}test{% endraw %}"
-- HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False}
-- >>> eval "{%raw%}test{%endraw%}"
-- {% raw %}test{% endraw %}
-- >>> eval "{% raw %}{{ test }}{% endraw %}"
-- {% raw %}{{ test }}{% endraw %}
-- >>> eval "  {% raw %}  test  {% endraw %}"
-- {% raw %}  test  {% endraw %}
-- >>> exec "  {% raw %}  test  {% endraw %}"
-- HaijiParserState {haijiParserStateLeadingSpaces = Just   , haijiParserStateInExtends = False}
-- >>> eval "  {%- raw -%}   test  {%- endraw -%}  "
-- {% raw %}test{% endraw %}
-- >>> exec "  {%- raw -%}   test  {%- endraw -%}  "
-- HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False}
--
rawParser :: HaijiParser AST
rawParser = withLeadingSpacesOf startRaw restRaw where
  startRaw = statement $ string "raw"
  restRaw _ = do
    (raw, leadingEndRawSpaces) <- till (liftParser anyChar) (statement (string "endraw") >> getLeadingSpaces)
    return $ Raw $ raw ++ maybe "" show leadingEndRawSpaces where
      till :: Alternative f => f a -> f b -> f ([a], b)
      till p end = go where
        go = ((,) [] <$> end) <|> ((\a (as,b) -> (a:as, b)) <$> p <*> go)

-- |
--
-- >>> let eval = either error id . parseOnly (evalHaijiParser extendsParser)
-- >>> let exec = either error id . parseOnly (execHaijiParser extendsParser)
-- >>> eval "{% extends \"foo.tmpl\" %}"
-- {% extends "foo.tmpl" %}
-- >>> exec "{% extends \"foo.tmpl\" %}"
-- HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False}
-- >>> eval "{%extends\"foo.tmpl\"%}"
-- {% extends "foo.tmpl" %}
-- >>> eval "{% extends 'foo.tmpl' %}"
-- {% extends "foo.tmpl" %}
-- >>> eval "  {% extends \"foo.tmpl\" %}"
-- {% extends "foo.tmpl" %}
-- >>> exec "  {% extends \"foo.tmpl\" %}"
-- HaijiParserState {haijiParserStateLeadingSpaces = Just   , haijiParserStateInExtends = False}
-- >>> eval "  {%- extends \"foo.tmpl\" -%}   "
-- {% extends "foo.tmpl" %}
-- >>> exec "  {%- extends \"foo.tmpl\" -%}   "
-- HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False}
--
extendsParser :: HaijiParser AST
extendsParser = statement $ string "extends" >> skipSpace >> Extends . T.unpack <$> (quotedBy '"' <|> quotedBy '\'') where
  quotedBy c = char c *> takeTill (== c) <* char c -- TODO: ここもっとマジメにやらないと

-- |
--
-- >>> let eval = either error id . parseOnly (evalHaijiParser blockParser)
-- >>> let exec = either error id . parseOnly (execHaijiParser blockParser)
-- >>> eval "{% block foo %}テスト{% endblock %}"
-- {% block foo %}テスト{% endblock %}
-- >>> exec "{% block foo %}テスト{% endblock %}"
-- HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False}
-- >>> eval "{% block foo %}テスト{% endblock foo %}"
-- {% block foo %}テスト{% endblock %}
-- >>> eval "{% block foo %}テスト{% endblock bar %}"
-- *** Exception: Failed reading: blockParser
-- >>> eval "{%block foo%}テスト{%endblock%}"
-- {% block foo %}テスト{% endblock %}
-- >>> eval "{% blockfoo %}テスト{% endblock %}"
-- *** Exception: Failed reading: takeWith
-- >>> eval "    {% block foo %}テスト{% endblock %}"
-- {% block foo %}テスト{% endblock %}
-- >>> exec "    {% block foo %}テスト{% endblock %}"
-- HaijiParserState {haijiParserStateLeadingSpaces = Just     , haijiParserStateInExtends = False}
-- >>> eval "    {%- block foo -%}    テスト    {%- endblock -%}    "
-- {% block foo %}テスト{% endblock %}
-- >>> exec "    {%- block foo -%}    テスト    {%- endblock -%}    "
-- HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False}
--
blockParser :: HaijiParser AST
blockParser = withLeadingSpacesOf startBlock restBlock where
  startBlock = statement $ string "block" >> skipMany1 space >> identifier
  restBlock name = do
    body <- haijiParser
    mayEndName <- statement $ string "endblock" >> option Nothing (Just <$> (skipMany1 space >> identifier))
    leadingEndBlockSpaces <- getLeadingSpaces
    if maybe True (name ==) mayEndName
      then return $ Block name False (body ++ maybeToList leadingEndBlockSpaces)
      else fail "blockParser"

-- |
--
-- >>> let eval = either error id . parseOnly (evalHaijiParser commentParser)
-- >>> let exec = either error id . parseOnly (execHaijiParser commentParser)
-- >>> eval "{# comment #}"
-- ()
-- >>> exec "{# comment #}"
-- HaijiParserState {haijiParserStateLeadingSpaces = Nothing, haijiParserStateInExtends = False}
-- >>> eval "  {# comment #}"
-- ()
-- >>> exec "  {# comment #}"
-- HaijiParserState {haijiParserStateLeadingSpaces = Just   , haijiParserStateInExtends = False}
--
commentParser :: HaijiParser ()
commentParser = saveLeadingSpaces <* liftParser (string "{#" >> manyTill anyChar (string "#}"))
