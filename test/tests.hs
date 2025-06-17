{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,9,0)
{-# LANGUAGE TypeApplications #-}
#endif
module Main ( main ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ( (<$>) )
#endif
import Control.Monad
import Text.Haiji
import Text.Haiji.Runtime
import Data.Aeson
import Data.Default
#if MIN_VERSION_base(4,11,0)
#else
import Data.Monoid
#endif
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.Exit
import System.Process.Text.Lazy
import Test.Tasty.TH
import Test.Tasty.HUnit

import GHC.Records

main :: IO ()
main = $(defaultMainGenerator)

jinja2 :: ToJSON a => FilePath -> a -> IO LT.Text
jinja2 template dict = do
  (code, out, err) <- readProcessWithExitCode "uv" ["run", "python3"] script
  unless (code == ExitSuccess) $ LT.putStrLn err
  return out where
    script = LT.unlines
             [ "import json"
             , "from jinja2 import Environment, PackageLoader"
             , "env = Environment(loader=PackageLoader('example', '.'),autoescape=True)"
             , "template = env.get_template('" <> LT.pack template <> "')"
             , "object = json.loads(" <> (LT.pack $ show $ BL.unpack $ encode dict) <> ")"
             , "print(template.render(object),end='')"
             , "exit()"
             ]

data CaseExampleNavigationDict = CaseExampleNavigationDict {
  _caseExampleNavigationDict_caption :: T.Text,
  _caseExampleNavigationDict_href :: T.Text
} deriving (Show, Eq)
instance HasField "caption" CaseExampleNavigationDict T.Text where
  getField = _caseExampleNavigationDict_caption
instance HasField "href" CaseExampleNavigationDict T.Text where
  getField = _caseExampleNavigationDict_href
instance ToJSON CaseExampleNavigationDict where
  toJSON x =
    object [ "caption" .= _caseExampleNavigationDict_caption x
           , "href" .= _caseExampleNavigationDict_href x
           ]

data CaseExampleDict = CaseExampleDict {
  _caseExampleDict_a_variable :: T.Text,
  _caseExampleDict_navigation :: [CaseExampleNavigationDict],
  _caseExampleDict_foo :: Integer,
  _caseExampleDict_bar :: T.Text
} deriving (Show, Eq)

instance HasField "a_variable" CaseExampleDict T.Text where
  getField = _caseExampleDict_a_variable
instance HasField "navigation" CaseExampleDict [CaseExampleNavigationDict] where
  getField = _caseExampleDict_navigation
instance HasField "foo" CaseExampleDict Integer where
  getField = _caseExampleDict_foo
instance HasField "bar" CaseExampleDict T.Text where
  getField = _caseExampleDict_bar
instance ToJSON CaseExampleDict where
  toJSON x =
    object [ "a_variable" .= _caseExampleDict_a_variable x
           , "navigation" .= _caseExampleDict_navigation x
           , "foo" .= _caseExampleDict_foo x
           , "bar" .= _caseExampleDict_bar x
           ]

case_example :: Assertion
case_example = do
  expected <- jinja2 "example.tmpl" dict
  expected @=? render $(haijiFile def "example.tmpl") dict
  tmpl <- readTemplateFile def "example.tmpl"
  expected @=? render tmpl (toJSON dict)
    where
      dict = CaseExampleDict {
        _caseExampleDict_a_variable = "Hello,World!",
        _caseExampleDict_navigation = [
            CaseExampleNavigationDict {
              _caseExampleNavigationDict_caption = "A",
              _caseExampleNavigationDict_href = "content/a.html"
            }
          , CaseExampleNavigationDict {
              _caseExampleNavigationDict_caption = "B",
              _caseExampleNavigationDict_href = "content/b.html"
            }
          ],
        _caseExampleDict_foo = 1,
        _caseExampleDict_bar = ""
      }


case_empty :: Assertion
case_empty = do
  expected <- jinja2 "test/empty.tmpl" ()
  tmpl <- readTemplateFile def "test/empty.tmpl"
  expected @=? render tmpl (toJSON ())
  expected @=? render $(haijiFile def "test/empty.tmpl") ()

case_lf1 :: Assertion
case_lf1 = do
  expected <- jinja2 "test/lf1.tmpl" ()
  tmpl <- readTemplateFile def "test/lf1.tmpl"
  expected @=? render tmpl (toJSON ())
  expected @=? render $(haijiFile def "test/lf1.tmpl") ()

case_lf2 :: Assertion
case_lf2 = do
  expected <- jinja2 "test/lf2.tmpl" ()
  tmpl <- readTemplateFile def "test/lf2.tmpl"
  expected @=? render tmpl (toJSON ())
  expected @=? render $(haijiFile def "test/lf2.tmpl") ()

case_line_without_newline :: Assertion
case_line_without_newline = do
  expected <- jinja2 "test/line_without_newline.tmpl" ()
  tmpl <- readTemplateFile def "test/line_without_newline.tmpl"
  expected @=? render tmpl (toJSON ())
  expected @=? render $(haijiFile def "test/line_without_newline.tmpl") ()

case_line_with_newline :: Assertion
case_line_with_newline = do
  expected <- jinja2 "test/line_with_newline.tmpl" ()
  tmpl <- readTemplateFile def "test/line_with_newline.tmpl"
  expected @=? render tmpl (toJSON ())
  expected @=? render $(haijiFile def "test/line_with_newline.tmpl") ()

data CaseVariablesDict = CaseVariablesDict {
  _caseVariablesDict_foo :: T.Text,
  _caseVariablesDict__foo :: T.Text,
  _caseVariablesDict_Foo :: T.Text,
  _caseVariablesDict_F__o_o__ :: T.Text,
  _caseVariablesDict_F1a2b3c :: T.Text
} deriving (Show, Eq)
instance HasField "foo" CaseVariablesDict T.Text where
  getField = _caseVariablesDict_foo
instance HasField "_foo" CaseVariablesDict T.Text where
  getField = _caseVariablesDict__foo
instance HasField "Foo" CaseVariablesDict T.Text where
  getField = _caseVariablesDict_Foo
instance HasField "F__o_o__" CaseVariablesDict T.Text where
  getField = _caseVariablesDict_F__o_o__
instance HasField "F1a2b3c" CaseVariablesDict T.Text where
  getField = _caseVariablesDict_F1a2b3c
instance ToJSON CaseVariablesDict where
  toJSON x =
    object [ "foo" .=  _caseVariablesDict_foo x
           , "_foo" .=  _caseVariablesDict__foo x
           , "Foo" .= _caseVariablesDict_Foo x
           , "F__o_o__" .= _caseVariablesDict_F__o_o__ x
           , "F1a2b3c" .= _caseVariablesDict_F1a2b3c x
           ]

case_variables :: Assertion
case_variables = do
  expected <- jinja2 "test/variables.tmpl" dict
  tmpl <- readTemplateFile def "test/variables.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/variables.tmpl") dict
    where
      dict = CaseVariablesDict {
        _caseVariablesDict_foo = "normal",
        _caseVariablesDict__foo = "start '_'",
        _caseVariablesDict_Foo = "start upper case",
        _caseVariablesDict_F__o_o__ = "include '_'",
        _caseVariablesDict_F1a2b3c = "include num"
      }

data CaseStringDict = CaseStringDict {
  _caseStringDict_test :: T.Text
} deriving (Show, Eq)
instance HasField "tet" CaseStringDict T.Text where
  getField = _caseStringDict_test
instance ToJSON CaseStringDict where
  toJSON x =
    object [ "test" .=  _caseStringDict_test x
           ]

case_string :: Assertion
case_string = do
  expected <- jinja2 "test/string.tmpl" dict
  tmpl <- readTemplateFile def "test/string.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/string.tmpl") dict
    where
      dict = CaseStringDict {
        _caseStringDict_test = "test"
      }


data CaseArithDict = CaseArithDict {
  _caseArithDict_value :: Integer,
  _caseArithDict_array :: [Integer]
} deriving (Show, Eq)
instance HasField "value" CaseArithDict Integer where
  getField = _caseArithDict_value
instance HasField "array" CaseArithDict [Integer] where
  getField = _caseArithDict_array
instance ToJSON CaseArithDict where
  toJSON x =
    object [ "value" .= _caseArithDict_value x
           , "array" .= _caseArithDict_array x
           ]

case_arith :: Assertion
case_arith = do
  expected <- jinja2 "test/arith.tmpl" dict
  tmpl <- readTemplateFile def "test/arith.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/arith.tmpl") dict
    where
      dict = CaseArithDict {
        _caseArithDict_value = -1,
        _caseArithDict_array = [1,2,3]
      }

data CaseComparisonDict = CaseComparisonDict {
  _caseComparisonDict_value :: Integer,
  _caseComparisonDict_array :: [Integer],
  _caseComparisonDict_text :: T.Text
} deriving (Show, Eq)
instance HasField "value" CaseComparisonDict Integer where
  getField = _caseComparisonDict_value
instance HasField "array" CaseComparisonDict [Integer] where
  getField = _caseComparisonDict_array
instance HasField "text" CaseComparisonDict T.Text where
  getField = _caseComparisonDict_text
instance ToJSON CaseComparisonDict where
  toJSON x =
    object [ "value" .= _caseComparisonDict_value x
           , "array" .= _caseComparisonDict_array x
           , "text" .= _caseComparisonDict_text x
           ]

case_comparison :: Assertion
case_comparison = do
  expected <- jinja2 "test/comparison.tmpl" dict
  tmpl <- readTemplateFile def "test/comparison.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/comparison.tmpl") dict
    where
      dict = CaseComparisonDict {
        _caseComparisonDict_value = (1 :: Integer),
        _caseComparisonDict_array = [1,2,3],
        _caseComparisonDict_text = "text"
      }


data CaseLogicDict = CaseLogicDict {
  _caseLogicDict_value :: Integer,
  _caseLogicDict_array :: [Integer]
} deriving (Show, Eq)
instance HasField "value" CaseLogicDict Integer where
  getField = _caseLogicDict_value
instance HasField "array" CaseLogicDict [Integer] where
  getField = _caseLogicDict_array
instance ToJSON CaseLogicDict where
  toJSON x =
    object [ "value" .= _caseLogicDict_value x
           , "array" .= _caseLogicDict_array x
           ]

case_logic :: Assertion
case_logic = do
  expected <- jinja2 "test/logic.tmpl" dict
  tmpl <- readTemplateFile def "test/logic.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/logic.tmpl") dict
    where
      dict = CaseLogicDict {
        _caseLogicDict_value = 1,
        _caseLogicDict_array = [1,2,3]
      }


data CaseHTMLEscapeDict = CaseHTMLEscapeDict {
  _caseHTMLEscapeDict_foo :: T.Text
} deriving (Show, Eq)
instance HasField "foo" CaseHTMLEscapeDict T.Text where
  getField = _caseHTMLEscapeDict_foo
instance ToJSON CaseHTMLEscapeDict where
  toJSON x =
    object [ "foo" .= _caseHTMLEscapeDict_foo x
           ]

case_HTML_escape :: Assertion
case_HTML_escape = do
  expected <- jinja2 "test/HTML_escape.tmpl" dict
  tmpl <- readTemplateFile def "test/HTML_escape.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/HTML_escape.tmpl") dict
    where
      dict = CaseHTMLEscapeDict $ T.pack [' '..'\126']


data CaseConditionDict = CaseConditionDict {
  _caseConditionDict_foo :: Bool,
  _caseConditionDict_bar :: Bool,
  _caseConditionDict_baz :: Bool
} deriving (Show, Eq)
instance HasField "foo" CaseConditionDict Bool where
  getField = _caseConditionDict_foo
instance HasField "bar" CaseConditionDict Bool where
  getField = _caseConditionDict_bar
instance HasField "baz" CaseConditionDict Bool where
  getField = _caseConditionDict_baz
instance ToJSON CaseConditionDict where
  toJSON x =
    object [ "foo" .= _caseConditionDict_foo x
           , "bar" .= _caseConditionDict_bar x
           , "baz" .= _caseConditionDict_baz x
           ]

case_condition :: Assertion
case_condition = forM_ (replicateM 3 [True, False]) $ \[foo, bar, baz] -> do
  let dict = CaseConditionDict {
    _caseConditionDict_foo = foo,
    _caseConditionDict_bar = bar,
    _caseConditionDict_baz = baz
  }
  expected <- jinja2 "test/condition.tmpl" dict
  tmpl <- readTemplateFile def "test/condition.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/condition.tmpl") dict

data CaseForeachDict = CaseForeachDict {
  _caseForeachDict_foo :: [Integer],
  _caseForeachDict_bar :: T.Text
} deriving (Show, Eq)
instance HasField "foo" CaseForeachDict [Integer] where
  getField = _caseForeachDict_foo
instance HasField "bar" CaseForeachDict T.Text where
  getField = _caseForeachDict_bar
instance ToJSON CaseForeachDict where
  toJSON x =
    object [ "foo" .= _caseForeachDict_foo x
           , "bar" .= _caseForeachDict_bar x
           ]

case_foreach :: Assertion
case_foreach = do
  expected <- jinja2 "test/foreach.tmpl" dict
  tmpl <- readTemplateFile def "test/foreach.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/foreach.tmpl") dict
    where
      dict = CaseForeachDict {
        _caseForeachDict_foo = [0,2..10],
        _caseForeachDict_bar = "bar"
      }

case_foreach_shadowing :: Assertion
case_foreach_shadowing = do
  expected <- jinja2 "test/foreach.tmpl" dict
  tmpl <- readTemplateFile def "test/foreach.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/foreach.tmpl") dict
  False @=? ("bar" `LT.isInfixOf` expected)
    where
      dict = CaseForeachDict {
        _caseForeachDict_foo = [2,4..10],
        _caseForeachDict_bar = "bar"
      }

case_foreach_else_block :: Assertion
case_foreach_else_block = do
  expected <- jinja2 "test/foreach_else_block.tmpl" dict
  tmpl <- readTemplateFile def "test/foreach_else_block.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/foreach_else_block.tmpl") dict
    where
      dict = CaseForeachDict {
        _caseForeachDict_foo = [],
        _caseForeachDict_bar = "bar"
      }

data CaseIncludeDict a = CaseIncludeDict {
  _caseIncludeDict_foo :: [a]
} deriving (Show, Eq)
instance HasField "foo" (CaseIncludeDict a) [a] where
  getField = _caseIncludeDict_foo
instance ToJSON a => ToJSON (CaseIncludeDict a) where
  toJSON x =
    object [ "foo" .= _caseIncludeDict_foo x
           ]

case_include :: Assertion
case_include = do
  testInclude ([0..10] :: [Integer])
  testInclude (["","\n","\n\n"] :: [T.Text]) where
    testInclude xs = do
      expected <- jinja2 "test/include.tmpl" dict
      tmpl <- readTemplateFile def "test/include.tmpl"
      expected @=? render tmpl (toJSON dict)
      expected @=? render $(haijiFile def "test/include.tmpl") dict
        where
          dict = CaseIncludeDict {
            _caseIncludeDict_foo = xs
          }

data CaseRawDict = CaseRawDict {
  _caseRawDict_foo :: [Integer],
  _caseRawDict_bar :: T.Text
} deriving (Show, Eq)
instance HasField "foo" CaseRawDict [Integer] where
  getField = _caseRawDict_foo
instance HasField "bar" CaseRawDict T.Text where
  getField = _caseRawDict_bar
instance ToJSON CaseRawDict where
  toJSON x =
    object [ "foo" .= _caseRawDict_foo x
           , "bar" .= _caseRawDict_bar x
           ]

case_raw :: Assertion
case_raw = do
  expected <- jinja2 "test/raw.tmpl" dict
  tmpl <- readTemplateFile def "test/raw.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/raw.tmpl") dict
    where
      dict = CaseRawDict {
        _caseRawDict_foo = [0,2..10],
        _caseRawDict_bar = "bar"
      }


data CaseLoopVariablesDict = CaseLoopVariablesDict {
  _caseLoopVariablesDict_foo :: [Integer]
} deriving (Show, Eq)
instance HasField "foo" CaseLoopVariablesDict [Integer] where
  getField = _caseLoopVariablesDict_foo
instance ToJSON CaseLoopVariablesDict where
  toJSON x =
    object [ "foo" .= _caseLoopVariablesDict_foo x
           ]

case_loop_variables :: Assertion
case_loop_variables = do
  expected <- jinja2 "test/loop_variables.tmpl" dict
  tmpl <- readTemplateFile def "test/loop_variables.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/loop_variables.tmpl") dict
    where
      dict = CaseLoopVariablesDict [0,2..10]


data CaseWhitespaceControlDict = CaseWhitespaceControlDict {
  _caseWhitespaceControlDict_seq :: [Integer]
} deriving (Show, Eq)
instance HasField "seq" CaseWhitespaceControlDict [Integer] where
  getField = _caseWhitespaceControlDict_seq
instance ToJSON CaseWhitespaceControlDict where
  toJSON x =
    object [ "seq" .= _caseWhitespaceControlDict_seq x
           ]

case_whitespace_control :: Assertion
case_whitespace_control = do
  expected <- jinja2 "test/whitespace_control.tmpl" dict
  tmpl <- readTemplateFile def "test/whitespace_control.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/whitespace_control.tmpl") dict
    where
      dict = CaseWhitespaceControlDict [0,2..10]


data CaseCommentDict = CaseCommentDict {
  _caseCommentDict_seq :: [Integer]
} deriving (Show, Eq)
instance HasField "seq" CaseCommentDict [Integer] where
  getField = _caseCommentDict_seq
instance ToJSON CaseCommentDict where
  toJSON x =
    object [ "seq" .= _caseCommentDict_seq x
           ]

case_comment :: Assertion
case_comment = do
  expected <- jinja2 "test/comment.tmpl" dict
  tmpl <- readTemplateFile def "test/comment.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/comment.tmpl") dict
    where
      dict = CaseCommentDict [0,2..10]


data CaseSetDict = CaseSetDict {
  _caseSetDict_ys :: [Integer],
  _caseSetDict_xs :: [Integer]
} deriving (Show, Eq)
instance HasField "ys" CaseSetDict [Integer] where
  getField = _caseSetDict_ys
instance HasField "xs" CaseSetDict [Integer] where
  getField = _caseSetDict_xs
instance ToJSON CaseSetDict where
  toJSON x =
    object [ "ys" .= _caseSetDict_ys x
           , "xs" .= _caseSetDict_xs x
           ]

case_set :: Assertion
case_set = do
  expected <- jinja2 "test/set.tmpl" dict
  tmpl <- readTemplateFile def "test/set.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/set.tmpl") dict
    where
      dict = CaseSetDict {
        _caseSetDict_ys = [0..2] :: [Integer],
        _caseSetDict_xs = [0..3] :: [Integer]
      }


data CaseExtendsDict = CaseExtendsDict {
  _caseExtendsDict_foo :: T.Text,
  _caseExtendsDict_bar :: T.Text,
  _caseExtendsDict_baz :: T.Text
} deriving (Show, Eq)
instance HasField "foo" CaseExtendsDict T.Text where
  getField = _caseExtendsDict_foo
instance HasField "bar" CaseExtendsDict T.Text where
  getField = _caseExtendsDict_bar
instance HasField "baz" CaseExtendsDict T.Text where
  getField = _caseExtendsDict_baz
instance ToJSON CaseExtendsDict where
  toJSON x =
    object [ "foo" .= _caseExtendsDict_foo x
           , "bar" .= _caseExtendsDict_bar x
           , "baz" .= _caseExtendsDict_baz x
           ]

case_extends :: Assertion
case_extends = do
  expected <- jinja2 "test/child.tmpl" dict
  tmpl <- readTemplateFile def "test/child.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/child.tmpl") dict
    where
      dict = CaseExtendsDict {
        _caseExtendsDict_foo = "foo",
        _caseExtendsDict_bar = "bar",
        _caseExtendsDict_baz = "baz"
      }


data CaseManyVariables = CaseManyVariables
  { _caseManyVariables_a :: T.Text
  , _caseManyVariables_b :: T.Text
  , _caseManyVariables_c :: T.Text
  , _caseManyVariables_d :: T.Text
  , _caseManyVariables_e :: T.Text
  , _caseManyVariables_f :: T.Text
  , _caseManyVariables_g :: T.Text
  , _caseManyVariables_h :: T.Text
  , _caseManyVariables_i :: T.Text
  , _caseManyVariables_j :: T.Text
  , _caseManyVariables_k :: T.Text
  , _caseManyVariables_l :: T.Text
  , _caseManyVariables_m :: T.Text
  , _caseManyVariables_n :: T.Text
  , _caseManyVariables_o :: T.Text
  , _caseManyVariables_p :: T.Text
  , _caseManyVariables_q :: T.Text
  } deriving (Show, Eq)
instance HasField "a" CaseManyVariables T.Text where getField = _caseManyVariables_a
instance HasField "b" CaseManyVariables T.Text where getField = _caseManyVariables_b
instance HasField "c" CaseManyVariables T.Text where getField = _caseManyVariables_c
instance HasField "d" CaseManyVariables T.Text where getField = _caseManyVariables_d
instance HasField "e" CaseManyVariables T.Text where getField = _caseManyVariables_e
instance HasField "f" CaseManyVariables T.Text where getField = _caseManyVariables_f
instance HasField "g" CaseManyVariables T.Text where getField = _caseManyVariables_g
instance HasField "h" CaseManyVariables T.Text where getField = _caseManyVariables_h
instance HasField "i" CaseManyVariables T.Text where getField = _caseManyVariables_i
instance HasField "j" CaseManyVariables T.Text where getField = _caseManyVariables_j
instance HasField "k" CaseManyVariables T.Text where getField = _caseManyVariables_k
instance HasField "l" CaseManyVariables T.Text where getField = _caseManyVariables_l
instance HasField "m" CaseManyVariables T.Text where getField = _caseManyVariables_m
instance HasField "n" CaseManyVariables T.Text where getField = _caseManyVariables_n
instance HasField "o" CaseManyVariables T.Text where getField = _caseManyVariables_o
instance HasField "p" CaseManyVariables T.Text where getField = _caseManyVariables_p
instance HasField "q" CaseManyVariables T.Text where getField = _caseManyVariables_q
instance ToJSON CaseManyVariables where
  toJSON x = object
    [ "a" .= _caseManyVariables_a x
    , "b" .= _caseManyVariables_b x
    , "c" .= _caseManyVariables_c x
    , "d" .= _caseManyVariables_d x
    , "e" .= _caseManyVariables_e x
    , "f" .= _caseManyVariables_f x
    , "g" .= _caseManyVariables_g x
    , "h" .= _caseManyVariables_h x
    , "i" .= _caseManyVariables_i x
    , "j" .= _caseManyVariables_j x
    , "k" .= _caseManyVariables_k x
    , "l" .= _caseManyVariables_l x
    , "m" .= _caseManyVariables_m x
    , "n" .= _caseManyVariables_n x
    , "o" .= _caseManyVariables_o x
    , "p" .= _caseManyVariables_p x
    , "q" .= _caseManyVariables_q x
    ]

case_many_variables :: Assertion
case_many_variables = do
  expected <- jinja2 "test/many_variables.tmpl" dict --
  tmpl <- readTemplateFile def "test/many_variables.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/many_variables.tmpl") dict
    where
      dict = CaseManyVariables
             { _caseManyVariables_a = "b" :: T.Text
             , _caseManyVariables_b = "b" :: T.Text
             , _caseManyVariables_c = "b" :: T.Text
             , _caseManyVariables_d = "b" :: T.Text
             , _caseManyVariables_e = "b" :: T.Text
             , _caseManyVariables_f = "b" :: T.Text
             , _caseManyVariables_g = "b" :: T.Text
             , _caseManyVariables_h = "b" :: T.Text
             , _caseManyVariables_i = "b" :: T.Text
             , _caseManyVariables_j = "b" :: T.Text
             , _caseManyVariables_k = "b" :: T.Text
             , _caseManyVariables_l = "b" :: T.Text
             , _caseManyVariables_m = "b" :: T.Text
             , _caseManyVariables_n = "b" :: T.Text
             , _caseManyVariables_o = "b" :: T.Text
             , _caseManyVariables_p = "b" :: T.Text
             , _caseManyVariables_q = "b" :: T.Text
             }
