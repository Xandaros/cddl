{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.CDDL.Parser where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Control.Error
import Text.Megaparsec.Error (errorBundlePretty)
import Data.FileEmbed

import Text.ABNF.ABNF
import Text.ABNF.Document
import Data.CDDL.Types

import Lens.Micro

cddlAbnf :: Rule
cddlAbnf = case parseABNF "cddl.abnf" (Text.decodeUtf8 $(embedFile "cddl.abnf")) of
  Left msg -> error ("cddlAbnf: failed to parse cddl.abnf.\n" <> errorBundlePretty msg)
  Right rules -> case canonicalizeRules "cddl" rules of
    Nothing -> error "cddlAbnf: failed to canonicalize rules"
    Just rules' -> rules'

parseCDDL :: Text.Text -> Either String CDDL
parseCDDL cddl = do
  fromDocument =<< parseDocument cddlAbnf cddl

class FromDocument b a | a -> b where
    fromDocument :: Document b -> Either String a

instance FromDocument Text.Text CDDL where
    fromDocument doc@(Document "cddl" _) = do
        firstRule <- justErr "No rule found" $ doc ^? child "rule"
        typeName  <- justErr "First rule cannot be a group" $ firstRule ^? child "typename"
        ruleName  <- justErr "Malformed CDDL" $ typeName ^? child "id"
        CDDL (getContent ruleName) <$> fromDocument doc
    fromDocument _ = Left "Malformed CDDL"

instance FromDocument Text.Text Rules where
    fromDocument _ = Left "niy"

child :: Applicative f => Text.Text -> (Document a -> f (Document a)) -> Document a -> f (Document a)
child a f s@(Document x _) = if a == x then f s else pure s
child _ _ s = pure s
