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
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.Exit
import System.Process.Text.Lazy
import Test.Tasty.TH
import Test.Tasty.HUnit

main :: IO ()
main = $(defaultMainGenerator)

jinja2 :: Show a => FilePath -> a -> IO LT.Text
jinja2 template dict = do
  (code, out, err) <- readProcessWithExitCode "python3" [] script
  unless (code == ExitSuccess) $ LT.putStrLn err
  return out where
    script = LT.unlines
             [ "import json"
             , "from jinja2 import Environment, PackageLoader"
             , "env = Environment(loader=PackageLoader('example', '.'),autoescape=True)"
             , "template = env.get_template('" <> LT.pack template <> "')"
             , "object = json.loads(" <> LT.pack (show $ show dict) <> ")"
             , "print(template.render(object),end='')"
             , "exit()"
             ]

case_example :: Assertion
case_example = do
  expected <- jinja2 "example.tmpl" dict
  expected @=? render $(haijiFile def "example.tmpl") dict
  tmpl <- readTemplateFile def "example.tmpl"
  expected @=? render tmpl (toJSON dict)
    where
#if MIN_VERSION_base(4,9,0)
      dict = toDict @"a_variable" ("Hello,World!" :: T.Text) `merge`
             toDict @"navigation" [ toDict @"caption" ("A" :: T.Text) `merge`
                                    toDict @"href" ("content/a.html" :: T.Text)
                                  , toDict @"caption" "B" `merge`
                                    toDict @"href" "content/b.html"
                                  ] `merge`
             toDict @"foo" (1 :: Integer) `merge`
             toDict @"bar" ("" :: T.Text)
#else
      dict = [key|a_variable|] ("Hello,World!" :: T.Text) `merge`
             [key|navigation|] [ [key|caption|] ("A" :: T.Text) `merge`
                                 [key|href|] ("content/a.html" :: T.Text)
                               , [key|caption|] "B" `merge`
                                 [key|href|] "content/b.html"
                               ] `merge`
             [key|foo|] (1 :: Integer) `merge`
             [key|bar|] ("" :: T.Text)
#endif

case_empty :: Assertion
case_empty = do
  expected <- jinja2 "test/empty.tmpl" empty
  tmpl <- readTemplateFile def "test/empty.tmpl"
  expected @=? render tmpl (toJSON empty)
  expected @=? render $(haijiFile def "test/empty.tmpl") empty

case_lf1 :: Assertion
case_lf1 = do
  expected <- jinja2 "test/lf1.tmpl" empty
  tmpl <- readTemplateFile def "test/lf1.tmpl"
  expected @=? render tmpl (toJSON empty)
  expected @=? render $(haijiFile def "test/lf1.tmpl") empty

case_lf2 :: Assertion
case_lf2 = do
  expected <- jinja2 "test/lf2.tmpl" empty
  tmpl <- readTemplateFile def "test/lf2.tmpl"
  expected @=? render tmpl (toJSON empty)
  expected @=? render $(haijiFile def "test/lf2.tmpl") empty

case_line_without_newline :: Assertion
case_line_without_newline = do
  expected <- jinja2 "test/line_without_newline.tmpl" empty
  tmpl <- readTemplateFile def "test/line_without_newline.tmpl"
  expected @=? render tmpl (toJSON empty)
  expected @=? render $(haijiFile def "test/line_without_newline.tmpl") empty

case_line_with_newline :: Assertion
case_line_with_newline = do
  expected <- jinja2 "test/line_with_newline.tmpl" empty
  tmpl <- readTemplateFile def "test/line_with_newline.tmpl"
  expected @=? render tmpl (toJSON empty)
  expected @=? render $(haijiFile def "test/line_with_newline.tmpl") empty

case_variables :: Assertion
case_variables = do
  expected <- jinja2 "test/variables.tmpl" dict
  tmpl <- readTemplateFile def "test/variables.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/variables.tmpl") dict
    where
      dict = [key|foo|] ("normal" :: T.Text) `merge`
             [key|_foo|] ("start '_'" :: T.Text) `merge`
             [key|Foo|] ("start upper case" :: T.Text) `merge`
             [key|F__o_o__|] ("include '_'" :: T.Text) `merge`
             [key|F1a2b3c|] ("include num" :: T.Text)

case_string :: Assertion
case_string = do
  expected <- jinja2 "test/string.tmpl" dict
  tmpl <- readTemplateFile def "test/string.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/string.tmpl") dict
    where
      dict = [key|test|] ("test" :: T.Text)

case_range :: Assertion
case_range = do
  expected <- jinja2 "test/range.tmpl" dict
  tmpl <- readTemplateFile def "test/range.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/range.tmpl") dict
    where
      dict = [key|value|] (5 :: Integer) `merge`
             [key|array|] ([1,2,3] :: [Integer])

case_arith :: Assertion
case_arith = do
  expected <- jinja2 "test/arith.tmpl" dict
  tmpl <- readTemplateFile def "test/arith.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/arith.tmpl") dict
    where
      dict = [key|value|] ((-1) :: Integer) `merge`
             [key|array|] ([1,2,3] :: [Integer])

case_comparison :: Assertion
case_comparison = do
  expected <- jinja2 "test/comparison.tmpl" dict
  tmpl <- readTemplateFile def "test/comparison.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/comparison.tmpl") dict
    where
      dict = [key|value|] ((1) :: Integer) `merge` -- There exists jinja2 bug (https://github.com/pallets/jinja/issues/755)
             [key|array|] ([1,2,3] :: [Integer]) `merge`
             [key|text|] ("text" :: T.Text)



case_logic :: Assertion
case_logic = do
  expected <- jinja2 "test/logic.tmpl" dict
  tmpl <- readTemplateFile def "test/logic.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/logic.tmpl") dict
    where
      dict = [key|value|] ((1) :: Integer) `merge`
             [key|array|] ([1,2,3] :: [Integer])

case_HTML_escape :: Assertion
case_HTML_escape = do
  expected <- jinja2 "test/HTML_escape.tmpl" dict
  tmpl <- readTemplateFile def "test/HTML_escape.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/HTML_escape.tmpl") dict
    where
      dict = [key|foo|] (T.pack [' '..'\126'])

case_condition :: Assertion
case_condition = forM_ (replicateM 3 [True, False]) $ \[foo, bar, baz] -> do
  let dict = [key|foo|] foo `merge`
             [key|bar|] bar `merge`
             [key|baz|] baz
  expected <- jinja2 "test/condition.tmpl" dict
  tmpl <- readTemplateFile def "test/condition.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/condition.tmpl") dict

case_foreach :: Assertion
case_foreach = do
  expected <- jinja2 "test/foreach.tmpl" dict
  tmpl <- readTemplateFile def "test/foreach.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/foreach.tmpl") dict
    where
      dict = [key|foo|] ([0,2..10] :: [Integer])

case_foreach_shadowing :: Assertion
case_foreach_shadowing = do
  expected <- jinja2 "test/foreach.tmpl" dict
  tmpl <- readTemplateFile def "test/foreach.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/foreach.tmpl") dict
  False @=? ("bar" `LT.isInfixOf` expected)
    where
      dict = [key|foo|] ([0,2..10] :: [Integer]) `merge`
             [key|bar|] ("bar" :: T.Text)

case_foreach_else_block :: Assertion
case_foreach_else_block = do
  expected <- jinja2 "test/foreach_else_block.tmpl" dict
  tmpl <- readTemplateFile def "test/foreach_else_block.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/foreach_else_block.tmpl") dict
    where
      dict = [key|foo|] ([] :: [Integer])

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
          dict = [key|foo|] xs

case_raw :: Assertion
case_raw = do
  expected <- jinja2 "test/raw.tmpl" dict
  tmpl <- readTemplateFile def "test/raw.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/raw.tmpl") dict
    where
      dict = [key|foo|] ([0,2..10] :: [Integer]) `merge`
             [key|bar|] ("bar" :: T.Text)

case_loop_variables :: Assertion
case_loop_variables = do
  expected <- jinja2 "test/loop_variables.tmpl" dict
  tmpl <- readTemplateFile def "test/loop_variables.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/loop_variables.tmpl") dict
    where
      dict = [key|foo|] ([0,2..10] :: [Integer])

case_whitespace_control :: Assertion
case_whitespace_control = do
  expected <- jinja2 "test/whitespace_control.tmpl" dict
  tmpl <- readTemplateFile def "test/whitespace_control.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/whitespace_control.tmpl") dict
    where
      dict = [key|seq|] ([0,2..10] :: [Integer])

case_comment :: Assertion
case_comment = do
  expected <- jinja2 "test/comment.tmpl" dict
  tmpl <- readTemplateFile def "test/comment.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/comment.tmpl") dict
    where
      dict = [key|seq|] ([0,2..10] :: [Integer])

case_set :: Assertion
case_set = do
  expected <- jinja2 "test/set.tmpl" dict
  tmpl <- readTemplateFile def "test/set.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/set.tmpl") dict
    where
      dict = [key|ys|] ([0..2] :: [Integer]) `merge`
             [key|xs|] ([0..3] :: [Integer])

case_extends :: Assertion
case_extends = do
  expected <- jinja2 "test/child.tmpl" dict
  tmpl <- readTemplateFile def "test/child.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/child.tmpl") dict
    where
      dict = [key|foo|] ("foo" :: T.Text) `merge`
             [key|bar|] ("bar" :: T.Text) `merge`
             [key|baz|] ("baz" :: T.Text)

case_many_variables :: Assertion
case_many_variables = do
  expected <- jinja2 "test/many_variables.tmpl" dict --
  tmpl <- readTemplateFile def "test/many_variables.tmpl"
  expected @=? render tmpl (toJSON dict)
  expected @=? render $(haijiFile def "test/many_variables.tmpl") dict
    where
      dict = [key|a|] ("b" :: T.Text) `merge`
             [key|b|] ("b" :: T.Text) `merge`
             [key|c|] ("b" :: T.Text) `merge`
             [key|d|] ("b" :: T.Text) `merge`
             [key|e|] ("b" :: T.Text) `merge`
             [key|f|] ("b" :: T.Text) `merge`
             [key|g|] ("b" :: T.Text) `merge`
             [key|h|] ("b" :: T.Text) `merge`
             [key|i|] ("b" :: T.Text) `merge`
             [key|j|] ("b" :: T.Text) `merge`
             [key|k|] ("b" :: T.Text) `merge`
             [key|l|] ("b" :: T.Text) `merge`
             [key|m|] ("b" :: T.Text) `merge`
             [key|n|] ("b" :: T.Text) `merge`
             [key|o|] ("b" :: T.Text) `merge`
             [key|p|] ("b" :: T.Text) `merge`
             [key|q|] ("b" :: T.Text)
