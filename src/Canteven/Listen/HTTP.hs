{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Types for the canteven-serve-with-ekg library.
module Canteven.Listen.HTTP (
    ListenerConfig(..),
    Scheme(..),
    WithListenHTTPConfig(..),
    ) where

import Data.Aeson (FromJSON(parseJSON), withObject, (.:))
import GHC.Generics (Generic)

-- | A type representing the ports to listen
data WithListenHTTPConfig userConfig =
    WithListenHTTPConfig {
        listeners :: [ListenerConfig],
        userConfig :: userConfig
    } deriving (Generic)

instance (FromJSON c) => FromJSON (WithListenHTTPConfig c) where
    parseJSON v = flip (withObject "WithListenHTTPConfig") v $ \obj -> do
        listeners <- obj .: "listen"
        userConfig <- parseJSON v
        return WithListenHTTPConfig {
            listeners,
            userConfig
        }

-- | A type representing a single "listener".
--
-- An application can listen to many of these
data ListenerConfig =
    ListenerConfig {
        scheme :: Scheme,
        port :: Int,
        cert :: Maybe FilePath,
        key :: Maybe FilePath
    } deriving (Generic, Eq, Show)

instance FromJSON ListenerConfig


data Scheme = HTTP | HTTPS
    deriving (Generic, Eq, Show)

instance FromJSON Scheme
