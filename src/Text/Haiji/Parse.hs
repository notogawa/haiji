{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
module Text.Haiji.Parse
       ( Expression(..)
       , Variable(..)
       , AST(..)
       , Loaded(..)
       , Template(..)
       , parseString
       , parseFile
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.State.Strict
import Data.Attoparsec.Text
import Data.Char
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

import Text.Haiji.Syntax.Identifier
import Text.Haiji.Syntax.Variable
import Text.Haiji.Syntax.Expression

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> let execHaijiParser p = snd <$> runHaijiParser p

type Scoped = Bool

type Base = Bool

data Loaded = Fully
            | Partially

data AST :: Loaded -> * where
  Literal :: T.Text -> AST a
  Eval :: Expression -> AST a
  Condition :: Expression -> [AST a] -> Maybe [AST a] -> AST a
  Foreach :: Identifier -> Expression -> [AST a] -> Maybe [AST a] -> AST a
  Include :: FilePath -> AST 'Partially
  Raw :: String -> AST a
  Extends :: FilePath -> AST 'Partially
  Base :: [AST 'Fully] -> AST 'Fully
  Block :: Base -> Identifier -> Scoped -> [AST a] -> AST a
  Super :: AST a
  Comment :: String -> AST a

deriving instance Eq (AST a)

data Template =
  Template
  { templateBase :: [AST 'Fully]
  , templateChild :: [AST 'Fully]
  } deriving (Eq, Show)

toTemplate :: [AST 'Fully] -> Template
toTemplate (Base base : asts) = tmpl { templateChild = templateChild tmpl ++ asts } where
  tmpl = toTemplate base
toTemplate asts = Template { templateBase = asts, templateChild = [] }

parseString :: String -> IO Template
parseString = (toTemplate <$>) . either error readAllFile . parseOnly parser . T.pack

parseFileWith :: (LT.Text -> LT.Text) -> FilePath -> IO Template
parseFileWith f file = LT.readFile file >>= parseString . LT.unpack . f

readAllFile :: [AST 'Partially] -> IO [AST 'Fully]
readAllFile asts = concat <$> mapM parseFileRecursively asts

parseFileRecursively :: AST 'Partially -> IO [AST 'Fully]
parseFileRecursively (Literal l) = return [ Literal l ]
parseFileRecursively (Eval v) = return [ Eval v ]
parseFileRecursively (Condition p ts fs) =
  ((:[]) .) . Condition p
  <$> readAllFile ts
  <*> runMaybeT (maybe mzero return fs >>= lift . readAllFile)
parseFileRecursively (Foreach k xs loopBody elseBody) =
  ((:[]) .) . Foreach k xs
  <$> readAllFile loopBody
  <*> runMaybeT (maybe mzero return elseBody >>= lift . readAllFile)
parseFileRecursively (Include includeFile) = templateBase <$> parseIncludeFile includeFile
parseFileRecursively (Raw content) = return [ Raw content ]
parseFileRecursively (Extends extendsfile) = (:[]) . Base . templateBase <$> parseFile extendsfile
parseFileRecursively (Block base name scoped body) =
  (:[]) . Block base name scoped <$> readAllFile body
parseFileRecursively Super = return [ Super ]
parseFileRecursively (Comment c) = return [ Comment c ]

parseFile :: FilePath -> IO Template
parseFile = parseFileWith deleteLastOneLF where
  deleteLastOneLF :: LT.Text -> LT.Text
  deleteLastOneLF xs
    | "%}\n" `LT.isSuffixOf` xs     = LT.init xs
    | "\n\n" `LT.isSuffixOf` xs     = LT.init xs
    | not ("\n" `LT.isSuffixOf` xs) = xs `LT.append` "\n"
    | otherwise                     = xs

parseIncludeFile :: FilePath -> IO Template
parseIncludeFile = parseFileWith deleteLastOneLF where
  deleteLastOneLF xs
    | LT.null xs         = xs
    | LT.last xs == '\n' = LT.init xs
    | otherwise          = xs

instance Show (AST a) where
  show (Literal l) = T.unpack l
  show (Eval v) = "{{ " ++ shows v " }}"
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
  show (Raw content) = "{% raw %}" ++ content ++ "{% endraw %}"
  show (Extends file) = "{% extends \"" ++ file ++ "\" %}"
  show (Base asts) = concatMap show asts
  show (Block _ name scoped body) =
    "{% block " ++ show name ++ (if scoped then " scoped" else "") ++" %}" ++
    concatMap show body ++
    "{% endblock %}"
  show Super = "{{ super() }}"
  show (Comment c) = "{#" ++ c ++ "#}"

data ParserState =
  ParserState
  { parserStateLeadingSpaces :: Maybe (AST 'Partially)
  , parserStateInBaseTemplate :: Bool
  } deriving (Eq, Show)

defaultParserState :: ParserState
defaultParserState =
  ParserState
  { parserStateLeadingSpaces = Nothing
  , parserStateInBaseTemplate = True
  }

newtype HaijiParser a =
  HaijiParser
  { unHaijiParser :: StateT ParserState Parser a
  } deriving (Functor, Applicative, Alternative, Monad, MonadState ParserState)

runHaijiParser :: HaijiParser a -> Parser (a, ParserState)
runHaijiParser p = runStateT (unHaijiParser p) defaultParserState

evalHaijiParser :: HaijiParser a -> Parser a
evalHaijiParser p = fst <$> runHaijiParser p

liftParser :: Parser a -> HaijiParser a
liftParser = HaijiParser . lift

saveLeadingSpaces :: HaijiParser ()
saveLeadingSpaces = liftParser leadingSpaces >>= setLeadingSpaces where
  leadingSpaces = option Nothing (Just . Literal <$> takeWhile1 isSpace)

withLeadingSpacesOf :: HaijiParser a -> (a -> HaijiParser b) -> HaijiParser b
withLeadingSpacesOf p q = do
  a <- p
  getLeadingSpaces >>= (q a <*) . setLeadingSpaces

setLeadingSpaces :: Maybe (AST 'Partially) -> HaijiParser ()
setLeadingSpaces ss = modify (\s -> s { parserStateLeadingSpaces = ss })

resetLeadingSpaces :: HaijiParser ()
resetLeadingSpaces = setLeadingSpaces Nothing

getLeadingSpaces :: HaijiParser (Maybe (AST 'Partially))
getLeadingSpaces = gets parserStateLeadingSpaces

setWhetherBaseTemplate :: Bool -> HaijiParser ()
setWhetherBaseTemplate x = modify (\s -> s { parserStateInBaseTemplate = x })

getWhetherBaseTemplate :: HaijiParser Bool
getWhetherBaseTemplate = gets parserStateInBaseTemplate

parser :: Parser [AST 'Partially]
parser = evalHaijiParser (haijiParser <* liftParser endOfInput)

haijiParser :: HaijiParser [AST 'Partially]
haijiParser = concat <$> many (resetLeadingSpaces *> choice (map toList parsers)) where
  parsers = [ literal
            , evaluation
            , condition
            , foreach
            , include
            , raw
            , extends
            , block
            , super
            , comment
            ]
  toList p = do
    b <- p
    a <- getLeadingSpaces
    return $ maybe id (:) a [b]

-- |
--
-- >>> let eval = either (error "parse error") id . parseOnly (evalHaijiParser literal)
-- >>> eval "テスト{test"
-- テスト
-- >>> eval "   テスト  {test"
--    テスト
-- >>> eval "   テスト  {%-test"
--    テスト
-- >>> eval "   テスト  テスト  {%-test"
--    テスト  テスト
--
literal :: HaijiParser (AST 'Partially)
literal = liftParser $ Literal . T.concat <$> many1 go where
  go = do
    sp <- takeTill (not . isSpace)
    pc <- peekChar
    case pc of
      Nothing  -> if T.null sp then fail "literal" else return sp
      Just '{' -> fail "literal"
      _        -> T.append sp <$> takeWhile1 (\c -> c /= '{' && not (isSpace c))

-- |
--
-- >>> let eval = either (error "parse error") id . parseOnly (evalHaijiParser evaluation)
-- >>> let exec = either (error "parse error") id . parseOnly (execHaijiParser evaluation)
-- >>> eval "{{ foo }}"
-- {{ foo }}
-- >>> exec "{{ foo }}"
-- ParserState {parserStateLeadingSpaces = Nothing, parserStateInBaseTemplate = True}
-- >>> eval "{{bar}}"
-- {{ bar }}
-- >>> eval "{{   baz}}"
-- {{ baz }}
-- >>> eval " {{ foo }}"
-- {{ foo }}
-- >>> exec " {{ foo }}"
-- ParserState {parserStateLeadingSpaces = Just  , parserStateInBaseTemplate = True}
-- >>> eval "{ { foo }}"
-- *** Exception: parse error
-- >>> eval "{{ foo } }"
-- *** Exception: parse error
-- >>> eval "{{ foo }} "
-- {{ foo }}
--
evaluation :: HaijiParser (AST 'Partially)
evaluation = saveLeadingSpaces *> liftParser deref where
  deref = Eval <$> ((string "{{" >> skipSpace) *> expression <* (skipSpace >> string "}}"))

-- |
--
-- >>> let exec = either (error "parse error") id . parseOnly (execHaijiParser $ statement $ return ())
-- >>> exec "{%%}"
-- ParserState {parserStateLeadingSpaces = Nothing, parserStateInBaseTemplate = True}
-- >>> exec "{% %}"
-- ParserState {parserStateLeadingSpaces = Nothing, parserStateInBaseTemplate = True}
-- >>> exec " {% %} "
-- ParserState {parserStateLeadingSpaces = Just  , parserStateInBaseTemplate = True}
-- >>> exec " {%- -%} "
-- ParserState {parserStateLeadingSpaces = Nothing, parserStateInBaseTemplate = True}
--
statement :: Parser a -> HaijiParser a
statement f = start "{%" <|> (start "{%-" <* resetLeadingSpaces) where
  start s = saveLeadingSpaces *> liftParser ((string s  >> skipSpace) *> f <* (skipSpace >> end))
  end = string "%}" <|> (string "-%}" <* skipSpace)

-- |
--
-- >>> let eval = either (error "parse error") id . parseOnly (evalHaijiParser condition)
-- >>> let exec = either (error "parse error") id . parseOnly (execHaijiParser condition)
-- >>> eval "{% if foo %}テスト{% endif %}"
-- {% if foo %}テスト{% endif %}
-- >>> exec "{% if foo %}テスト{% endif %}"
-- ParserState {parserStateLeadingSpaces = Nothing, parserStateInBaseTemplate = True}
-- >>> eval "{%if foo%}テスト{%endif%}"
-- {% if foo %}テスト{% endif %}
-- >>> eval "{% iffoo %}テスト{% endif %}"
-- *** Exception: parse error
-- >>> eval "{% if foo %}真{% else %}偽{% endif %}"
-- {% if foo %}真{% else %}偽{% endif %}
-- >>> eval "{%if foo%}{%if bar%}{%else%}{%endif%}{%else%}{%if baz%}{%else%}{%endif%}{%endif%}"
-- {% if foo %}{% if bar %}{% else %}{% endif %}{% else %}{% if baz %}{% else %}{% endif %}{% endif %}
-- >>> eval "    {% if foo %}テスト{% endif %}"
-- {% if foo %}テスト{% endif %}
-- >>> exec "    {% if foo %}テスト{% endif %}"
-- ParserState {parserStateLeadingSpaces = Just     , parserStateInBaseTemplate = True}
-- >>> eval "    {%- if foo -%}    テスト    {%- endif -%}    "
-- {% if foo %}テスト{% endif %}
-- >>> exec "    {%- if foo -%}    テスト    {%- endif -%}    "
-- ParserState {parserStateLeadingSpaces = Nothing, parserStateInBaseTemplate = True}
--
condition :: HaijiParser (AST 'Partially)
condition = withLeadingSpacesOf start rest where
  start = statement $ string "if" >> skipMany1 space >> expression
  rest cond = do
    ifPart <- haijiParser
    mElsePart <- mayElse
    leadingElseSpaces <- getLeadingSpaces
    _ <- statement $ string "endif"
    leadingEndIfSpaces <- getLeadingSpaces
    return $ case mElsePart of
      Nothing       -> Condition cond (ifPart ++ maybeToList leadingEndIfSpaces) Nothing
      Just elsePart -> Condition cond (ifPart ++ maybeToList leadingElseSpaces ) (Just $ elsePart ++ maybeToList leadingEndIfSpaces)

mayElse :: HaijiParser (Maybe [AST 'Partially])
mayElse = option Nothing (Just <$> elseParser) where
  elseParser = withLeadingSpacesOf (statement (string "else")) $ const haijiParser

-- |
--
-- >>> let eval = either (error "parse error") id . parseOnly (evalHaijiParser foreach)
-- >>> let exec = either (error "parse error") id . parseOnly (execHaijiParser foreach)
-- >>> eval "{% for _ in foo %}loop{% endfor %}"
-- {% for _ in foo %}loop{% endfor %}
-- >>> exec "{% for _ in foo %}loop{% endfor %}"
-- ParserState {parserStateLeadingSpaces = Nothing, parserStateInBaseTemplate = True}
-- >>> eval "{%for _ in foo%}loop{%endfor%}"
-- {% for _ in foo %}loop{% endfor %}
-- >>> eval "{% for_ in foo %}loop{% endfor %}"
-- *** Exception: parse error
-- >>> eval "{% for _in foo %}loop{% endfor %}"
-- *** Exception: parse error
-- >>> eval "{% for _ infoo %}loop{% endfor %}"
-- *** Exception: parse error
-- >>> eval "{% for _ in foo %}loop{% else %}else block{% endfor %}"
-- {% for _ in foo %}loop{% else %}else block{% endfor %}
-- >>> eval "{%for _ in foo%}loop{%else%}else block{%endfor%}"
-- {% for _ in foo %}loop{% else %}else block{% endfor %}
-- >>> eval "  {% for _ in foo %}  loop  {% endfor %}  "
-- {% for _ in foo %}  loop  {% endfor %}
-- >>> exec "  {% for _ in foo %}  loop  {% endfor %}  "
-- ParserState {parserStateLeadingSpaces = Just   , parserStateInBaseTemplate = True}
-- >>> eval "  {%- for _ in foo -%}  loop  {%- endfor -%}  "
-- {% for _ in foo %}loop{% endfor %}
-- >>> exec "  {%- for _ in foo -%}  loop  {%- endfor -%}  "
-- ParserState {parserStateLeadingSpaces = Nothing, parserStateInBaseTemplate = True}
--
foreach :: HaijiParser (AST 'Partially)
foreach = withLeadingSpacesOf start rest where
  start = statement $ Foreach
          <$> (string "for" >> skipMany1 space >> identifier)
          <*> (skipMany1 space >> string "in" >> skipMany1 space >> expression)
  rest f = do
    loopPart <- haijiParser
    mElsePart <- mayElse
    leadingElseSpaces <- getLeadingSpaces
    _ <- statement (string "endfor")
    leadingEndForSpaces <- getLeadingSpaces
    return $ case mElsePart of
      Nothing       -> f (loopPart ++ maybeToList leadingEndForSpaces) Nothing
      Just elsePart -> f (loopPart ++ maybeToList leadingElseSpaces  ) (Just $ elsePart ++ maybeToList leadingEndForSpaces)

-- |
--
-- >>> let eval = either (error "parse error") id . parseOnly (evalHaijiParser include)
-- >>> let exec = either (error "parse error") id . parseOnly (execHaijiParser include)
-- >>> eval "{% include \"foo.tmpl\" %}"
-- {% include "foo.tmpl" %}
-- >>> exec "{% include \"foo.tmpl\" %}"
-- ParserState {parserStateLeadingSpaces = Nothing, parserStateInBaseTemplate = True}
-- >>> eval "{%include\"foo.tmpl\"%}"
-- {% include "foo.tmpl" %}
-- >>> eval "{% include 'foo.tmpl' %}"
-- {% include "foo.tmpl" %}
-- >>> eval "  {% include \"foo.tmpl\" %}"
-- {% include "foo.tmpl" %}
-- >>> exec "  {% include \"foo.tmpl\" %}"
-- ParserState {parserStateLeadingSpaces = Just   , parserStateInBaseTemplate = True}
-- >>> eval "  {%- include \"foo.tmpl\" -%}   "
-- {% include "foo.tmpl" %}
-- >>> exec "  {%- include \"foo.tmpl\" -%}   "
-- ParserState {parserStateLeadingSpaces = Nothing, parserStateInBaseTemplate = True}
--
include :: HaijiParser (AST 'Partially)
include = statement $ string "include" >> skipSpace >> Include . T.unpack <$> (quotedBy '"' <|> quotedBy '\'') where
    quotedBy c = char c *> takeTill (== c) <* char c -- TODO: ここもっとマジメにやらないと

-- |
--
-- >>> let eval = either (error "parse error") id . parseOnly (evalHaijiParser raw)
-- >>> let exec = either (error "parse error") id . parseOnly (execHaijiParser raw)
-- >>> eval "{% raw %}test{% endraw %}"
-- {% raw %}test{% endraw %}
-- >>> exec "{% raw %}test{% endraw %}"
-- ParserState {parserStateLeadingSpaces = Nothing, parserStateInBaseTemplate = True}
-- >>> eval "{%raw%}test{%endraw%}"
-- {% raw %}test{% endraw %}
-- >>> eval "{% raw %}{{ test }}{% endraw %}"
-- {% raw %}{{ test }}{% endraw %}
-- >>> eval "  {% raw %}  test  {% endraw %}"
-- {% raw %}  test  {% endraw %}
-- >>> exec "  {% raw %}  test  {% endraw %}"
-- ParserState {parserStateLeadingSpaces = Just   , parserStateInBaseTemplate = True}
-- >>> eval "  {%- raw -%}   test  {%- endraw -%}  "
-- {% raw %}test{% endraw %}
-- >>> exec "  {%- raw -%}   test  {%- endraw -%}  "
-- ParserState {parserStateLeadingSpaces = Nothing, parserStateInBaseTemplate = True}
--
raw :: HaijiParser (AST 'Partially)
raw = withLeadingSpacesOf start rest where
  start = statement $ string "raw"
  rest _ = do
    (content, leadingEndRawSpaces) <- till (liftParser anyChar) (statement (string "endraw") >> getLeadingSpaces)
    return $ Raw $ content ++ maybe "" show leadingEndRawSpaces where
      till :: Alternative f => f a -> f b -> f ([a], b)
      till p end = go where
        go = ((,) [] <$> end) <|> ((\a (as,b) -> (a:as, b)) <$> p <*> go)

-- |
--
-- >>> let eval = either (error "parse error") id . parseOnly (evalHaijiParser extends)
-- >>> let exec = either (error "parse error") id . parseOnly (execHaijiParser extends)
-- >>> eval "{% extends \"foo.tmpl\" %}"
-- {% extends "foo.tmpl" %}
-- >>> exec "{% extends \"foo.tmpl\" %}"
-- ParserState {parserStateLeadingSpaces = Nothing, parserStateInBaseTemplate = False}
-- >>> eval "{%extends\"foo.tmpl\"%}"
-- {% extends "foo.tmpl" %}
-- >>> eval "{% extends 'foo.tmpl' %}"
-- {% extends "foo.tmpl" %}
-- >>> eval "  {% extends \"foo.tmpl\" %}"
-- {% extends "foo.tmpl" %}
-- >>> exec "  {% extends \"foo.tmpl\" %}"
-- ParserState {parserStateLeadingSpaces = Just   , parserStateInBaseTemplate = False}
-- >>> eval "  {%- extends \"foo.tmpl\" -%}   "
-- {% extends "foo.tmpl" %}
-- >>> exec "  {%- extends \"foo.tmpl\" -%}   "
-- ParserState {parserStateLeadingSpaces = Nothing, parserStateInBaseTemplate = False}
--
extends :: HaijiParser (AST 'Partially)
extends = do
  base <- getWhetherBaseTemplate
  unless base $ fail "extends"
  go <* setWhetherBaseTemplate False where
    go = statement $ string "extends" >> skipSpace >> Extends . T.unpack <$> (quotedBy '"' <|> quotedBy '\'')
    quotedBy c = char c *> takeTill (== c) <* char c -- TODO: ここもっとマジメにやらないと

-- |
--
-- >>> let eval = either (error "parse error") id . parseOnly (evalHaijiParser block)
-- >>> let exec = either (error "parse error") id . parseOnly (execHaijiParser block)
-- >>> eval "{% block foo %}テスト{% endblock %}"
-- {% block foo %}テスト{% endblock %}
-- >>> exec "{% block foo %}テスト{% endblock %}"
-- ParserState {parserStateLeadingSpaces = Nothing, parserStateInBaseTemplate = True}
-- >>> eval "{% block foo %}テスト{% endblock foo %}"
-- {% block foo %}テスト{% endblock %}
-- >>> eval "{% block foo %}テスト{% endblock bar %}"
-- *** Exception: parse error
-- >>> eval "{%block foo%}テスト{%endblock%}"
-- {% block foo %}テスト{% endblock %}
-- >>> eval "{% blockfoo %}テスト{% endblock %}"
-- *** Exception: parse error
-- >>> eval "    {% block foo %}テスト{% endblock %}"
-- {% block foo %}テスト{% endblock %}
-- >>> exec "    {% block foo %}テスト{% endblock %}"
-- ParserState {parserStateLeadingSpaces = Just     , parserStateInBaseTemplate = True}
-- >>> eval "    {%- block foo -%}    テスト    {%- endblock -%}    "
-- {% block foo %}テスト{% endblock %}
-- >>> exec "    {%- block foo -%}    テスト    {%- endblock -%}    "
-- ParserState {parserStateLeadingSpaces = Nothing, parserStateInBaseTemplate = True}
--
block :: HaijiParser (AST 'Partially)
block = withLeadingSpacesOf start rest where
  start = statement $ string "block" >> skipMany1 space >> identifier
  rest name = do
    body <- haijiParser
    mayEndName <- statement $ string "endblock" >> option Nothing (Just <$> (skipMany1 space >> identifier))
    leadingEndBlockSpaces <- getLeadingSpaces
    base <- getWhetherBaseTemplate
    if maybe True (name ==) mayEndName
      then return $ Block base name False (body ++ maybeToList leadingEndBlockSpaces)
      else fail "block"

super :: HaijiParser (AST 'Partially)
super = do
  saveLeadingSpaces
  _ <- liftParser ((string "{{" *> skipSpace) *>
                   (string "super" *> skipSpace >> char '(' >> skipSpace >> char ')') <*
                   (skipSpace *> string "}}"))
  return Super

-- |
--
-- >>> let eval = either (error "parse error") id . parseOnly (evalHaijiParser comment)
-- >>> let exec = either (error "parse error") id . parseOnly (execHaijiParser comment)
-- >>> eval "{# comment #}"
-- {# comment #}
-- >>> exec "{# comment #}"
-- ParserState {parserStateLeadingSpaces = Nothing, parserStateInBaseTemplate = True}
-- >>> eval "  {# comment #}"
-- {# comment #}
-- >>> exec "  {# comment #}"
-- ParserState {parserStateLeadingSpaces = Just   , parserStateInBaseTemplate = True}
--
comment :: HaijiParser (AST 'Partially)
comment = saveLeadingSpaces *> liftParser (string "{#" >> Comment <$> manyTill anyChar (string "#}"))
