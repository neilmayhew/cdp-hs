{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= DigitalCredentials

This domain allows interacting with the Digital Credentials API for automation.
-}


module CDP.Domains.DigitalCredentials (module CDP.Domains.DigitalCredentials) where

import           Control.Applicative  ((<$>))
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.Trans  (liftIO)
import qualified Data.Map             as M
import           Data.Maybe          
import Data.Functor.Identity
import Data.String
import qualified Data.Text as T
import qualified Data.List as List
import qualified Data.Text.IO         as TI
import qualified Data.Vector          as V
import Data.Aeson.Types (Parser(..))
import           Data.Aeson           (FromJSON (..), ToJSON (..), (.:), (.:?), (.=), (.!=), (.:!))
import qualified Data.Aeson           as A
import qualified Network.HTTP.Simple as Http
import qualified Network.URI          as Uri
import qualified Network.WebSockets as WS
import Control.Concurrent
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Proxy
import System.Random
import GHC.Generics
import Data.Char
import Data.Default

import CDP.Internal.Utils


import CDP.Domains.DOMNetworkEmulationPageSecurity as DOMNetworkEmulationPageSecurity


-- | Type 'DigitalCredentials.VirtualWalletAction'.
--   The type of virtual wallet action.
data DigitalCredentialsVirtualWalletAction = DigitalCredentialsVirtualWalletActionRespond | DigitalCredentialsVirtualWalletActionDecline | DigitalCredentialsVirtualWalletActionWait | DigitalCredentialsVirtualWalletActionClear
  deriving (Ord, Eq, Show, Read)
instance FromJSON DigitalCredentialsVirtualWalletAction where
  parseJSON = A.withText "DigitalCredentialsVirtualWalletAction" $ \v -> case v of
    "respond" -> pure DigitalCredentialsVirtualWalletActionRespond
    "decline" -> pure DigitalCredentialsVirtualWalletActionDecline
    "wait" -> pure DigitalCredentialsVirtualWalletActionWait
    "clear" -> pure DigitalCredentialsVirtualWalletActionClear
    "_" -> fail "failed to parse DigitalCredentialsVirtualWalletAction"
instance ToJSON DigitalCredentialsVirtualWalletAction where
  toJSON v = A.String $ case v of
    DigitalCredentialsVirtualWalletActionRespond -> "respond"
    DigitalCredentialsVirtualWalletActionDecline -> "decline"
    DigitalCredentialsVirtualWalletActionWait -> "wait"
    DigitalCredentialsVirtualWalletActionClear -> "clear"

-- | Sets the behavior of the virtual wallet for digital credential requests
--   issued from this frame.

-- | Parameters of the 'DigitalCredentials.setVirtualWalletBehavior' command.
data PDigitalCredentialsSetVirtualWalletBehavior = PDigitalCredentialsSetVirtualWalletBehavior
  {
    -- | The action of the virtual wallet.
    pDigitalCredentialsSetVirtualWalletBehaviorAction :: DigitalCredentialsVirtualWalletAction,
    -- | The protocol identifier (e.g. "openid4vp"). Required when |action| is
    --   "respond", forbidden otherwise.
    pDigitalCredentialsSetVirtualWalletBehaviorProtocol :: Maybe T.Text,
    -- | The response data object returned by the wallet.
    --   Required when |action| is "respond", forbidden otherwise.
    pDigitalCredentialsSetVirtualWalletBehaviorResponse :: Maybe [(T.Text, T.Text)],
    -- | The frame to scope the virtual wallet behavior to.
    pDigitalCredentialsSetVirtualWalletBehaviorFrameId :: Maybe DOMNetworkEmulationPageSecurity.PageFrameId
  }
  deriving (Eq, Show)
pDigitalCredentialsSetVirtualWalletBehavior
  {-
  -- | The action of the virtual wallet.
  -}
  :: DigitalCredentialsVirtualWalletAction
  -> PDigitalCredentialsSetVirtualWalletBehavior
pDigitalCredentialsSetVirtualWalletBehavior
  arg_pDigitalCredentialsSetVirtualWalletBehaviorAction
  = PDigitalCredentialsSetVirtualWalletBehavior
    arg_pDigitalCredentialsSetVirtualWalletBehaviorAction
    Nothing
    Nothing
    Nothing
instance ToJSON PDigitalCredentialsSetVirtualWalletBehavior where
  toJSON p = A.object $ catMaybes [
    ("action" A..=) <$> Just (pDigitalCredentialsSetVirtualWalletBehaviorAction p),
    ("protocol" A..=) <$> (pDigitalCredentialsSetVirtualWalletBehaviorProtocol p),
    ("response" A..=) <$> (pDigitalCredentialsSetVirtualWalletBehaviorResponse p),
    ("frameId" A..=) <$> (pDigitalCredentialsSetVirtualWalletBehaviorFrameId p)
    ]
instance Command PDigitalCredentialsSetVirtualWalletBehavior where
  type CommandResponse PDigitalCredentialsSetVirtualWalletBehavior = ()
  commandName _ = "DigitalCredentials.setVirtualWalletBehavior"
  fromJSON = const . A.Success . const ()

