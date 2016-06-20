{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.CDDL.Parser where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Control.Error
import Control.Monad.Trans (liftIO)
import Text.Megaparsec.Error (parseErrorPretty)
import System.Exit (exitFailure)

import Text.ABNF.ABNF
import Text.ABNF.Document
import Data.CDDL.Types

import Debug.Trace

parseCDDL :: Text.Text -> ExceptT String IO CDDL
parseCDDL cddl = do
    abnf  <- liftIO $ Text.readFile "cddl.abnf"
    abnf' <- case parseABNF "cddl.abnf" abnf of
               Left msg -> liftIO $ putStrLn (parseErrorPretty msg)
                           >> exitFailure
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
        firstRule <- headErr "No rule found" $ lookupDocument' "rule" doc
        typeName  <- headErr "First rule cannot be a group" $ lookupDocument' "typename" firstRule
        ruleName  <- headErr "Malformed CDDL" $ lookupDocument' "id" typeName
        traceM (Text.unpack $ getContent ruleName)
        CDDL (getContent ruleName) <$> fromDocument doc
    fromDocument _ = Left "Malformed CDDL"

instance FromDocument Text.Text Rules where
    fromDocument _ = Left "niy"

runNonTerminal :: Document a -> Either String (Document a)
runNonTerminal (Terminal _) = Left ""
runNonTerminal doc@(Document _ _) = Right doc

