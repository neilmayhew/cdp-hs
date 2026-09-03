{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= FedCm

This domain allows interacting with the FedCM dialog.
-}


module CDP.Domains.FedCm (module CDP.Domains.FedCm) where

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




-- | Type 'FedCm.LoginState'.
--   Whether this is a sign-up or sign-in action for this account, i.e.
--   whether this account has ever been used to sign in to this RP before.
data FedCmLoginState = FedCmLoginStateSignIn | FedCmLoginStateSignUp
  deriving (Ord, Eq, Show, Read)
instance FromJSON FedCmLoginState where
  parseJSON = A.withText "FedCmLoginState" $ \v -> case v of
    "SignIn" -> pure FedCmLoginStateSignIn
    "SignUp" -> pure FedCmLoginStateSignUp
    "_" -> fail "failed to parse FedCmLoginState"
instance ToJSON FedCmLoginState where
  toJSON v = A.String $ case v of
    FedCmLoginStateSignIn -> "SignIn"
    FedCmLoginStateSignUp -> "SignUp"

-- | Type 'FedCm.DialogType'.
--   The types of FedCM dialogs.
data FedCmDialogType = FedCmDialogTypeAccountChooser | FedCmDialogTypeAutoReauthn | FedCmDialogTypeConfirmIdpLogin | FedCmDialogTypeError
  deriving (Ord, Eq, Show, Read)
instance FromJSON FedCmDialogType where
  parseJSON = A.withText "FedCmDialogType" $ \v -> case v of
    "AccountChooser" -> pure FedCmDialogTypeAccountChooser
    "AutoReauthn" -> pure FedCmDialogTypeAutoReauthn
    "ConfirmIdpLogin" -> pure FedCmDialogTypeConfirmIdpLogin
    "Error" -> pure FedCmDialogTypeError
    "_" -> fail "failed to parse FedCmDialogType"
instance ToJSON FedCmDialogType where
  toJSON v = A.String $ case v of
    FedCmDialogTypeAccountChooser -> "AccountChooser"
    FedCmDialogTypeAutoReauthn -> "AutoReauthn"
    FedCmDialogTypeConfirmIdpLogin -> "ConfirmIdpLogin"
    FedCmDialogTypeError -> "Error"

-- | Type 'FedCm.DialogButton'.
--   The buttons on the FedCM dialog.
data FedCmDialogButton = FedCmDialogButtonConfirmIdpLoginContinue | FedCmDialogButtonErrorGotIt | FedCmDialogButtonErrorMoreDetails
  deriving (Ord, Eq, Show, Read)
instance FromJSON FedCmDialogButton where
  parseJSON = A.withText "FedCmDialogButton" $ \v -> case v of
    "ConfirmIdpLoginContinue" -> pure FedCmDialogButtonConfirmIdpLoginContinue
    "ErrorGotIt" -> pure FedCmDialogButtonErrorGotIt
    "ErrorMoreDetails" -> pure FedCmDialogButtonErrorMoreDetails
    "_" -> fail "failed to parse FedCmDialogButton"
instance ToJSON FedCmDialogButton where
  toJSON v = A.String $ case v of
    FedCmDialogButtonConfirmIdpLoginContinue -> "ConfirmIdpLoginContinue"
    FedCmDialogButtonErrorGotIt -> "ErrorGotIt"
    FedCmDialogButtonErrorMoreDetails -> "ErrorMoreDetails"

-- | Type 'FedCm.AccountUrlType'.
--   The URLs that each account has
data FedCmAccountUrlType = FedCmAccountUrlTypeTermsOfService | FedCmAccountUrlTypePrivacyPolicy
  deriving (Ord, Eq, Show, Read)
instance FromJSON FedCmAccountUrlType where
  parseJSON = A.withText "FedCmAccountUrlType" $ \v -> case v of
    "TermsOfService" -> pure FedCmAccountUrlTypeTermsOfService
    "PrivacyPolicy" -> pure FedCmAccountUrlTypePrivacyPolicy
    "_" -> fail "failed to parse FedCmAccountUrlType"
instance ToJSON FedCmAccountUrlType where
  toJSON v = A.String $ case v of
    FedCmAccountUrlTypeTermsOfService -> "TermsOfService"
    FedCmAccountUrlTypePrivacyPolicy -> "PrivacyPolicy"

-- | Type 'FedCm.Account'.
--   Corresponds to IdentityRequestAccount
data FedCmAccount = FedCmAccount
  {
    fedCmAccountAccountId :: T.Text,
    fedCmAccountEmail :: T.Text,
    fedCmAccountName :: T.Text,
    fedCmAccountGivenName :: T.Text,
    fedCmAccountPictureUrl :: T.Text,
    fedCmAccountIdpConfigUrl :: T.Text,
    fedCmAccountIdpLoginUrl :: T.Text,
    fedCmAccountLoginState :: FedCmLoginState,
    -- | These two are only set if the loginState is signUp
    fedCmAccountTermsOfServiceUrl :: Maybe T.Text,
    fedCmAccountPrivacyPolicyUrl :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON FedCmAccount where
  parseJSON = A.withObject "FedCmAccount" $ \o -> FedCmAccount
    <$> o A..: "accountId"
    <*> o A..: "email"
    <*> o A..: "name"
    <*> o A..: "givenName"
    <*> o A..: "pictureUrl"
    <*> o A..: "idpConfigUrl"
    <*> o A..: "idpLoginUrl"
    <*> o A..: "loginState"
    <*> o A..:? "termsOfServiceUrl"
    <*> o A..:? "privacyPolicyUrl"
instance ToJSON FedCmAccount where
  toJSON p = A.object $ catMaybes [
    ("accountId" A..=) <$> Just (fedCmAccountAccountId p),
    ("email" A..=) <$> Just (fedCmAccountEmail p),
    ("name" A..=) <$> Just (fedCmAccountName p),
    ("givenName" A..=) <$> Just (fedCmAccountGivenName p),
    ("pictureUrl" A..=) <$> Just (fedCmAccountPictureUrl p),
    ("idpConfigUrl" A..=) <$> Just (fedCmAccountIdpConfigUrl p),
    ("idpLoginUrl" A..=) <$> Just (fedCmAccountIdpLoginUrl p),
    ("loginState" A..=) <$> Just (fedCmAccountLoginState p),
    ("termsOfServiceUrl" A..=) <$> (fedCmAccountTermsOfServiceUrl p),
    ("privacyPolicyUrl" A..=) <$> (fedCmAccountPrivacyPolicyUrl p)
    ]

-- | Type of the 'FedCm.dialogShown' event.
data FedCmDialogShown = FedCmDialogShown
  {
    fedCmDialogShownDialogId :: T.Text,
    fedCmDialogShownDialogType :: FedCmDialogType,
    fedCmDialogShownAccounts :: [FedCmAccount],
    -- | These exist primarily so that the caller can verify the
    --   RP context was used appropriately.
    fedCmDialogShownTitle :: T.Text,
    fedCmDialogShownSubtitle :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON FedCmDialogShown where
  parseJSON = A.withObject "FedCmDialogShown" $ \o -> FedCmDialogShown
    <$> o A..: "dialogId"
    <*> o A..: "dialogType"
    <*> o A..: "accounts"
    <*> o A..: "title"
    <*> o A..:? "subtitle"
instance Event FedCmDialogShown where
  eventName _ = "FedCm.dialogShown"

-- | Type of the 'FedCm.dialogClosed' event.
data FedCmDialogClosed = FedCmDialogClosed
  {
    fedCmDialogClosedDialogId :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON FedCmDialogClosed where
  parseJSON = A.withObject "FedCmDialogClosed" $ \o -> FedCmDialogClosed
    <$> o A..: "dialogId"
instance Event FedCmDialogClosed where
  eventName _ = "FedCm.dialogClosed"


-- | Parameters of the 'FedCm.enable' command.
data PFedCmEnable = PFedCmEnable
  {
    -- | Allows callers to disable the promise rejection delay that would
    --   normally happen, if this is unimportant to what's being tested.
    --   (step 4 of https://fedidcg.github.io/FedCM/#browser-api-rp-sign-in)
    pFedCmEnableDisableRejectionDelay :: Maybe Bool
  }
  deriving (Eq, Show)
pFedCmEnable
  :: PFedCmEnable
pFedCmEnable
  = PFedCmEnable
    Nothing
instance ToJSON PFedCmEnable where
  toJSON p = A.object $ catMaybes [
    ("disableRejectionDelay" A..=) <$> (pFedCmEnableDisableRejectionDelay p)
    ]
instance Command PFedCmEnable where
  type CommandResponse PFedCmEnable = ()
  commandName _ = "FedCm.enable"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'FedCm.disable' command.
data PFedCmDisable = PFedCmDisable
  deriving (Eq, Show)
pFedCmDisable
  :: PFedCmDisable
pFedCmDisable
  = PFedCmDisable
instance ToJSON PFedCmDisable where
  toJSON _ = A.Null
instance Command PFedCmDisable where
  type CommandResponse PFedCmDisable = ()
  commandName _ = "FedCm.disable"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'FedCm.selectAccount' command.
data PFedCmSelectAccount = PFedCmSelectAccount
  {
    pFedCmSelectAccountDialogId :: T.Text,
    pFedCmSelectAccountAccountIndex :: Int
  }
  deriving (Eq, Show)
pFedCmSelectAccount
  :: T.Text
  -> Int
  -> PFedCmSelectAccount
pFedCmSelectAccount
  arg_pFedCmSelectAccountDialogId
  arg_pFedCmSelectAccountAccountIndex
  = PFedCmSelectAccount
    arg_pFedCmSelectAccountDialogId
    arg_pFedCmSelectAccountAccountIndex
instance ToJSON PFedCmSelectAccount where
  toJSON p = A.object $ catMaybes [
    ("dialogId" A..=) <$> Just (pFedCmSelectAccountDialogId p),
    ("accountIndex" A..=) <$> Just (pFedCmSelectAccountAccountIndex p)
    ]
instance Command PFedCmSelectAccount where
  type CommandResponse PFedCmSelectAccount = ()
  commandName _ = "FedCm.selectAccount"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'FedCm.clickDialogButton' command.
data PFedCmClickDialogButton = PFedCmClickDialogButton
  {
    pFedCmClickDialogButtonDialogId :: T.Text,
    pFedCmClickDialogButtonDialogButton :: FedCmDialogButton
  }
  deriving (Eq, Show)
pFedCmClickDialogButton
  :: T.Text
  -> FedCmDialogButton
  -> PFedCmClickDialogButton
pFedCmClickDialogButton
  arg_pFedCmClickDialogButtonDialogId
  arg_pFedCmClickDialogButtonDialogButton
  = PFedCmClickDialogButton
    arg_pFedCmClickDialogButtonDialogId
    arg_pFedCmClickDialogButtonDialogButton
instance ToJSON PFedCmClickDialogButton where
  toJSON p = A.object $ catMaybes [
    ("dialogId" A..=) <$> Just (pFedCmClickDialogButtonDialogId p),
    ("dialogButton" A..=) <$> Just (pFedCmClickDialogButtonDialogButton p)
    ]
instance Command PFedCmClickDialogButton where
  type CommandResponse PFedCmClickDialogButton = ()
  commandName _ = "FedCm.clickDialogButton"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'FedCm.openUrl' command.
data PFedCmOpenUrl = PFedCmOpenUrl
  {
    pFedCmOpenUrlDialogId :: T.Text,
    pFedCmOpenUrlAccountIndex :: Int,
    pFedCmOpenUrlAccountUrlType :: FedCmAccountUrlType
  }
  deriving (Eq, Show)
pFedCmOpenUrl
  :: T.Text
  -> Int
  -> FedCmAccountUrlType
  -> PFedCmOpenUrl
pFedCmOpenUrl
  arg_pFedCmOpenUrlDialogId
  arg_pFedCmOpenUrlAccountIndex
  arg_pFedCmOpenUrlAccountUrlType
  = PFedCmOpenUrl
    arg_pFedCmOpenUrlDialogId
    arg_pFedCmOpenUrlAccountIndex
    arg_pFedCmOpenUrlAccountUrlType
instance ToJSON PFedCmOpenUrl where
  toJSON p = A.object $ catMaybes [
    ("dialogId" A..=) <$> Just (pFedCmOpenUrlDialogId p),
    ("accountIndex" A..=) <$> Just (pFedCmOpenUrlAccountIndex p),
    ("accountUrlType" A..=) <$> Just (pFedCmOpenUrlAccountUrlType p)
    ]
instance Command PFedCmOpenUrl where
  type CommandResponse PFedCmOpenUrl = ()
  commandName _ = "FedCm.openUrl"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'FedCm.dismissDialog' command.
data PFedCmDismissDialog = PFedCmDismissDialog
  {
    pFedCmDismissDialogDialogId :: T.Text,
    pFedCmDismissDialogTriggerCooldown :: Maybe Bool
  }
  deriving (Eq, Show)
pFedCmDismissDialog
  :: T.Text
  -> PFedCmDismissDialog
pFedCmDismissDialog
  arg_pFedCmDismissDialogDialogId
  = PFedCmDismissDialog
    arg_pFedCmDismissDialogDialogId
    Nothing
instance ToJSON PFedCmDismissDialog where
  toJSON p = A.object $ catMaybes [
    ("dialogId" A..=) <$> Just (pFedCmDismissDialogDialogId p),
    ("triggerCooldown" A..=) <$> (pFedCmDismissDialogTriggerCooldown p)
    ]
instance Command PFedCmDismissDialog where
  type CommandResponse PFedCmDismissDialog = ()
  commandName _ = "FedCm.dismissDialog"
  fromJSON = const . A.Success . const ()

-- | Resets the cooldown time, if any, to allow the next FedCM call to show
--   a dialog even if one was recently dismissed by the user.

-- | Parameters of the 'FedCm.resetCooldown' command.
data PFedCmResetCooldown = PFedCmResetCooldown
  deriving (Eq, Show)
pFedCmResetCooldown
  :: PFedCmResetCooldown
pFedCmResetCooldown
  = PFedCmResetCooldown
instance ToJSON PFedCmResetCooldown where
  toJSON _ = A.Null
instance Command PFedCmResetCooldown where
  type CommandResponse PFedCmResetCooldown = ()
  commandName _ = "FedCm.resetCooldown"
  fromJSON = const . A.Success . const ()

