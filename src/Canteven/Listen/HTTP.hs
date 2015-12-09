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
import Data.Functor ((<$>))
import GHC.Generics (Generic)

-- | Toplevel type to parse listeners out of a config file.
data ListenHTTPConfig =
    ListenHTTPConfig {
        listeners :: [ListenerConfig]
    } deriving (Generic)

instance FromJSON ListenHTTPConfig where
    parseJSON = withObject "WithListenHTTPConfig" $ \obj ->
        ListenHTTPConfig <$> obj .: "listen"

-- | A type that adds a ListenHTTPConfig to any other config.
data WithListenHTTPConfig userConfig =
    WithListenHTTPConfig {
        listenConfig :: ListenHTTPConfig,
        userConfig :: userConfig
    } deriving (Generic)

instance (FromJSON c) => FromJSON (WithListenHTTPConfig c) where
    parseJSON v = do
        listenConfig <- parseJSON v
        userConfig <- parseJSON v
        return WithListenHTTPConfig {
            listenConfig,
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
