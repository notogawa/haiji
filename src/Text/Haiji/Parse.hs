{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Text.Haiji.Parse
       ( Jinja2(..)
       , parseString
       , parseFile
       ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

import Text.Haiji.Syntax

data Jinja2 =
  Jinja2
  { jinja2Base :: [AST 'Fully]
  , jinja2Child :: [AST 'Fully]
  } deriving (Eq, Show)

toJinja2 :: [AST 'Fully] -> Jinja2
toJinja2 (Base base : asts) = tmpl { jinja2Child = jinja2Child tmpl ++ asts } where
  tmpl = toJinja2 base
toJinja2 asts = Jinja2 { jinja2Base = asts, jinja2Child = [] }

parseString :: String -> IO Jinja2
parseString = (toJinja2 <$>) . either error readAllFile . parseOnly parser . T.pack

parseFileWith :: (LT.Text -> LT.Text) -> FilePath -> IO Jinja2
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
parseFileRecursively (Include includeFile) = jinja2Base <$> parseIncludeFile includeFile
parseFileRecursively (Raw content) = return [ Raw content ]
parseFileRecursively (Extends extendsfile) = (:[]) . Base . jinja2Base <$> parseFile extendsfile
parseFileRecursively (Block base name scoped body) =
  (:[]) . Block base name scoped <$> readAllFile body
parseFileRecursively Super = return [ Super ]
parseFileRecursively (Comment c) = return [ Comment c ]

parseFile :: FilePath -> IO Jinja2
parseFile = parseFileWith deleteLastOneLF where
  deleteLastOneLF :: LT.Text -> LT.Text
  deleteLastOneLF xs
    | "%}\n" `LT.isSuffixOf` xs     = LT.init xs
    | "\n\n" `LT.isSuffixOf` xs     = LT.init xs
    | not ("\n" `LT.isSuffixOf` xs) = xs `LT.append` "\n"
    | otherwise                     = xs

parseIncludeFile :: FilePath -> IO Jinja2
parseIncludeFile = parseFileWith deleteLastOneLF where
  deleteLastOneLF xs
    | LT.null xs         = xs
    | LT.last xs == '\n' = LT.init xs
    | otherwise          = xs
