{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Haiji.Syntax.Filter
       ( Filter(..)
       , filter
       ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif
import Data.Attoparsec.Text
import Prelude hiding (filter)

-- http://jinja.pocoo.org/docs/dev/templates/#builtin-filters
data Filter = FilterAbs
{-
            | FilterAttr Identifier
            | FilterBatch
            | FilterCapitalize
            | FilterCenter Int
            | FilterDefault
            | FilterDictSort
            | FilterEscape
            | FilterFileSizeFormat
            | FilterFirst
            | FilterFloat
            | FilterForceEscape
            | FilterFormat
            -- | FilterGroupBy Variable
            | FilterIndent Int Bool
            | FilterInt Int Int
            | FilterJoin String (Maybe Identifier)
            | FilterLast
            | FilterLength
            | FilterList
            | FilterLower
            | FilterMap
            | FilterPprint
            | FilterRandom
            | FilterReject
            | FilterRejectAttr
            | FilterReplace
            | FilterReverse
            | FilterRound
            | FilterSafe
            | FilterSelect
            | FilterSelectAttr
            | FilterSlice
            | FilterSort
            | FilterString
            | FilterStripTags
            | FilterSum
            | FilterTitle
            | FilterTrim
            | FilterTruncate
            | FilterUpper
            | FilterURLEncode
            | FilterURLize
            | FilterWordCount
            | FilterWordWrap
            | FilterXMLAttr
-}
            deriving Eq

instance Show Filter where
  show (FilterAbs) = "|abs"

filter :: Parser Filter
filter = char '|' *>
         skipSpace *>
         choice
         [ filterAbs
         ]

filterAbs :: Parser Filter
filterAbs = string "abs" *> return FilterAbs
