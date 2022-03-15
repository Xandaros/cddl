{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}

module Data.CDDL.Parser where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Control.Error
import Control.Monad.Trans (liftIO)
import Text.Megaparsec.Error (errorBundlePretty)
import System.Exit (exitFailure)

import Text.ABNF.ABNF
import Text.ABNF.Document
import Data.CDDL.Types

import Lens.Micro

import Debug.Trace

parseCDDL :: Text.Text -> ExceptT String IO CDDL
parseCDDL cddl = do
    abnf  <- liftIO $ Text.readFile "cddl.abnf"
    abnf' <- case parseABNF "cddl.abnf" abnf of
               Left msg -> liftIO $ do putStrLn (errorBundlePretty msg)
                                       exitFailure
               Right rules -> pure rules
    canon <- tryJust "could not canonicalise cddl.abnf"
             (canonicalizeRules "cddl" abnf')
    doc   <- tryRight $ parseDocument canon cddl
    liftIO $ print doc
    tryRight $ fromDocument doc

class FromDocument b a | a -> b where
    fromDocument :: Document b -> Either String a

instance FromDocument Text.Text CDDL where
    fromDocument doc@(Document "cddl" _) = do
        firstRule <- justErr "No rule found" $ doc ^? child "rule"
        typeName  <- justErr "First rule cannot be a group" $ firstRule ^? child "typename"
        ruleName  <- justErr "Malformed CDDL" $ typeName ^? child "id"
        traceM (Text.unpack $ getContent ruleName)
        CDDL (getContent ruleName) <$> fromDocument doc
    fromDocument _ = Left "Malformed CDDL"

instance FromDocument Text.Text Rules where
    fromDocument _ = Left "niy"

child :: Applicative f => Text.Text -> (Document a -> f (Document a)) -> Document a -> f (Document a)
child a f s@(Document x _) = if a == x then f s else pure s
child _ _ s = pure s
