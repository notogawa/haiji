{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Text.Haiji.Parse
       ( Template(..)
       , parseString
       , parseFile
       ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

import Text.Haiji.Syntax

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
