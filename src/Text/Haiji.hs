-- |
-- Module      : Text.Haiji
-- Copyright   : 2015 Noriyuki OHKAWA
-- License     : BSD3
--
-- Maintainer  : n.ohkawa@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Haiji is a template engine which is subset of Jinja2.
-- This is designed to free from the unintended rendering result by strictly typed variable interpolation.
--
-- Rendering result will be same as Jinja2's one. However, Haiji doesn't aim to be Jinja2.
-- Some feature and built-in Test\/Function\/Filter of Jinja2 allow rendering time type inspection.
-- Haiji will not support these type unsafe features.
-- Haiji generates a statically typed template by Template Haskell,
-- and check that a given dictionary includes enough information to render template.
--
-- >{-# LANGUAGE OverloadedStrings #-}
-- >{-# LANGUAGE TemplateHaskell #-}
-- >{-# LANGUAGE QuasiQuotes #-}
-- >{-# LANGUAGE DataKinds #-}
-- >module Main where
-- >
-- >import Data.Default
-- >import Text.Haiji
-- >import qualified Data.Text as T
-- >import qualified Data.Text.Lazy as LT
-- >import qualified Data.Text.Lazy.IO as LT
-- >
-- >main :: IO ()
-- >main = LT.putStr
-- >       $ render $(haijiFile def "example.tmpl")
-- >       $ [key|a_variable|] ("Hello,World!" :: LT.Text) `merge`
-- >         [key|navigation|] [ [key|caption|] cap `merge` [key|href|] href
-- >                           | (cap, href) <- [ ("A", "content/a.html")
-- >                                            , ("B", "content/b.html")
-- >                                            ] :: [ (T.Text, String) ]
-- >                           ] `merge`
-- >         [key|foo|] (1 :: Int) `merge`
-- >         [key|bar|] ("" :: String)

module Text.Haiji
    ( -- * Typed Template
      -- $template
      Template
      -- ** Generators
    , haiji
    , haijiFile
      -- ** Renderer
    , render
      -- * Rendering Environment
    , Environment
    , autoEscape
      -- * Dictionary
    , Dict
    , toDict
    , (:->)
    , empty
      -- ** Builder
    , key
    , merge
    ) where

import Text.Haiji.TH
import Text.Haiji.Types
import Text.Haiji.Dictionary

-- $template
-- >{{ foo }}
--
-- For example, this Jinja2 template requires "foo".
-- A dictionary which provides a variable "foo" is required to render it.
-- If a variable "foo" does not exist in a given dictionary,
-- Jinja2 evaluates it to an empty string by default,
-- whereas haiji treats this case as compile error.
