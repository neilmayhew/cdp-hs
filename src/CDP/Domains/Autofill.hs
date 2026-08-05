{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Autofill

Defines commands and events for Autofill.
-}


module CDP.Domains.Autofill (module CDP.Domains.Autofill) where

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


-- | Type 'Autofill.CreditCard'.
data AutofillCreditCard = AutofillCreditCard
  {
    -- | 16-digit credit card number.
    autofillCreditCardNumber :: T.Text,
    -- | Name of the credit card owner.
    autofillCreditCardName :: T.Text,
    -- | 2-digit expiry month.
    autofillCreditCardExpiryMonth :: T.Text,
    -- | 4-digit expiry year.
    autofillCreditCardExpiryYear :: T.Text,
    -- | 3-digit card verification code.
    autofillCreditCardCvc :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON AutofillCreditCard where
  parseJSON = A.withObject "AutofillCreditCard" $ \o -> AutofillCreditCard
    <$> o A..: "number"
    <*> o A..: "name"
    <*> o A..: "expiryMonth"
    <*> o A..: "expiryYear"
    <*> o A..: "cvc"
instance ToJSON AutofillCreditCard where
  toJSON p = A.object $ catMaybes [
    ("number" A..=) <$> Just (autofillCreditCardNumber p),
    ("name" A..=) <$> Just (autofillCreditCardName p),
    ("expiryMonth" A..=) <$> Just (autofillCreditCardExpiryMonth p),
    ("expiryYear" A..=) <$> Just (autofillCreditCardExpiryYear p),
    ("cvc" A..=) <$> Just (autofillCreditCardCvc p)
    ]

-- | Type 'Autofill.AddressField'.
data AutofillAddressField = AutofillAddressField
  {
    -- | address field name, for example GIVEN_NAME.
    --   The full list of supported field names:
    --   https://source.chromium.org/chromium/chromium/src/+/main:components/autofill/core/browser/field_types.cc;l=38
    autofillAddressFieldName :: T.Text,
    -- | address field value, for example Jon Doe.
    autofillAddressFieldValue :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON AutofillAddressField where
  parseJSON = A.withObject "AutofillAddressField" $ \o -> AutofillAddressField
    <$> o A..: "name"
    <*> o A..: "value"
instance ToJSON AutofillAddressField where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (autofillAddressFieldName p),
    ("value" A..=) <$> Just (autofillAddressFieldValue p)
    ]

-- | Type 'Autofill.AddressFields'.
--   A list of address fields.
data AutofillAddressFields = AutofillAddressFields
  {
    autofillAddressFieldsFields :: [AutofillAddressField]
  }
  deriving (Eq, Show)
instance FromJSON AutofillAddressFields where
  parseJSON = A.withObject "AutofillAddressFields" $ \o -> AutofillAddressFields
    <$> o A..: "fields"
instance ToJSON AutofillAddressFields where
  toJSON p = A.object $ catMaybes [
    ("fields" A..=) <$> Just (autofillAddressFieldsFields p)
    ]

-- | Type 'Autofill.Address'.
data AutofillAddress = AutofillAddress
  {
    -- | fields and values defining an address.
    autofillAddressFields :: [AutofillAddressField]
  }
  deriving (Eq, Show)
instance FromJSON AutofillAddress where
  parseJSON = A.withObject "AutofillAddress" $ \o -> AutofillAddress
    <$> o A..: "fields"
instance ToJSON AutofillAddress where
  toJSON p = A.object $ catMaybes [
    ("fields" A..=) <$> Just (autofillAddressFields p)
    ]

-- | Type 'Autofill.AddressUI'.
--   Defines how an address can be displayed like in chrome://settings/addresses.
--   Address UI is a two dimensional array, each inner array is an "address information line", and when rendered in a UI surface should be displayed as such.
--   The following address UI for instance:
--   [[{name: "GIVE_NAME", value: "Jon"}, {name: "FAMILY_NAME", value: "Doe"}], [{name: "CITY", value: "Munich"}, {name: "ZIP", value: "81456"}]]
--   should allow the receiver to render:
--   Jon Doe
--   Munich 81456
data AutofillAddressUI = AutofillAddressUI
  {
    -- | A two dimension array containing the representation of values from an address profile.
    autofillAddressUIAddressFields :: [AutofillAddressFields]
  }
  deriving (Eq, Show)
instance FromJSON AutofillAddressUI where
  parseJSON = A.withObject "AutofillAddressUI" $ \o -> AutofillAddressUI
    <$> o A..: "addressFields"
instance ToJSON AutofillAddressUI where
  toJSON p = A.object $ catMaybes [
    ("addressFields" A..=) <$> Just (autofillAddressUIAddressFields p)
    ]

-- | Type 'Autofill.FillingStrategy'.
--   Specified whether a filled field was done so by using the html autocomplete attribute or autofill heuristics.
data AutofillFillingStrategy = AutofillFillingStrategyAutocompleteAttribute | AutofillFillingStrategyAutofillInferred
  deriving (Ord, Eq, Show, Read)
instance FromJSON AutofillFillingStrategy where
  parseJSON = A.withText "AutofillFillingStrategy" $ \v -> case v of
    "autocompleteAttribute" -> pure AutofillFillingStrategyAutocompleteAttribute
    "autofillInferred" -> pure AutofillFillingStrategyAutofillInferred
    "_" -> fail "failed to parse AutofillFillingStrategy"
instance ToJSON AutofillFillingStrategy where
  toJSON v = A.String $ case v of
    AutofillFillingStrategyAutocompleteAttribute -> "autocompleteAttribute"
    AutofillFillingStrategyAutofillInferred -> "autofillInferred"

-- | Type 'Autofill.FilledField'.
data AutofillFilledField = AutofillFilledField
  {
    -- | The type of the field, e.g text, password etc.
    autofillFilledFieldHtmlType :: T.Text,
    -- | the html id
    autofillFilledFieldId :: T.Text,
    -- | the html name
    autofillFilledFieldName :: T.Text,
    -- | the field value
    autofillFilledFieldValue :: T.Text,
    -- | The actual field type, e.g FAMILY_NAME
    autofillFilledFieldAutofillType :: T.Text,
    -- | The filling strategy
    autofillFilledFieldFillingStrategy :: AutofillFillingStrategy,
    -- | The frame the field belongs to
    autofillFilledFieldFrameId :: DOMNetworkEmulationPageSecurity.PageFrameId,
    -- | The form field's DOM node
    autofillFilledFieldFieldId :: DOMNetworkEmulationPageSecurity.DOMBackendNodeId
  }
  deriving (Eq, Show)
instance FromJSON AutofillFilledField where
  parseJSON = A.withObject "AutofillFilledField" $ \o -> AutofillFilledField
    <$> o A..: "htmlType"
    <*> o A..: "id"
    <*> o A..: "name"
    <*> o A..: "value"
    <*> o A..: "autofillType"
    <*> o A..: "fillingStrategy"
    <*> o A..: "frameId"
    <*> o A..: "fieldId"
instance ToJSON AutofillFilledField where
  toJSON p = A.object $ catMaybes [
    ("htmlType" A..=) <$> Just (autofillFilledFieldHtmlType p),
    ("id" A..=) <$> Just (autofillFilledFieldId p),
    ("name" A..=) <$> Just (autofillFilledFieldName p),
    ("value" A..=) <$> Just (autofillFilledFieldValue p),
    ("autofillType" A..=) <$> Just (autofillFilledFieldAutofillType p),
    ("fillingStrategy" A..=) <$> Just (autofillFilledFieldFillingStrategy p),
    ("frameId" A..=) <$> Just (autofillFilledFieldFrameId p),
    ("fieldId" A..=) <$> Just (autofillFilledFieldFieldId p)
    ]

-- | Type of the 'Autofill.addressFormFilled' event.
data AutofillAddressFormFilled = AutofillAddressFormFilled
  {
    -- | Information about the fields that were filled
    autofillAddressFormFilledFilledFields :: [AutofillFilledField],
    -- | An UI representation of the address used to fill the form.
    --   Consists of a 2D array where each child represents an address/profile line.
    autofillAddressFormFilledAddressUi :: AutofillAddressUI
  }
  deriving (Eq, Show)
instance FromJSON AutofillAddressFormFilled where
  parseJSON = A.withObject "AutofillAddressFormFilled" $ \o -> AutofillAddressFormFilled
    <$> o A..: "filledFields"
    <*> o A..: "addressUi"
instance Event AutofillAddressFormFilled where
  eventName _ = "Autofill.addressFormFilled"

-- | Trigger autofill on a form identified by the fieldId.
--   If the field and related form cannot be autofilled, returns an error.

-- | Parameters of the 'Autofill.trigger' command.
data PAutofillTrigger = PAutofillTrigger
  {
    -- | Identifies a field that serves as an anchor for autofill.
    pAutofillTriggerFieldId :: DOMNetworkEmulationPageSecurity.DOMBackendNodeId,
    -- | Identifies the frame that field belongs to.
    pAutofillTriggerFrameId :: Maybe DOMNetworkEmulationPageSecurity.PageFrameId,
    -- | Credit card information to fill out the form. Credit card data is not saved.  Mutually exclusive with `address`.
    pAutofillTriggerCard :: Maybe AutofillCreditCard,
    -- | Address to fill out the form. Address data is not saved. Mutually exclusive with `card`.
    pAutofillTriggerAddress :: Maybe AutofillAddress
  }
  deriving (Eq, Show)
pAutofillTrigger
  {-
  -- | Identifies a field that serves as an anchor for autofill.
  -}
  :: DOMNetworkEmulationPageSecurity.DOMBackendNodeId
  -> PAutofillTrigger
pAutofillTrigger
  arg_pAutofillTriggerFieldId
  = PAutofillTrigger
    arg_pAutofillTriggerFieldId
    Nothing
    Nothing
    Nothing
instance ToJSON PAutofillTrigger where
  toJSON p = A.object $ catMaybes [
    ("fieldId" A..=) <$> Just (pAutofillTriggerFieldId p),
    ("frameId" A..=) <$> (pAutofillTriggerFrameId p),
    ("card" A..=) <$> (pAutofillTriggerCard p),
    ("address" A..=) <$> (pAutofillTriggerAddress p)
    ]
instance Command PAutofillTrigger where
  type CommandResponse PAutofillTrigger = ()
  commandName _ = "Autofill.trigger"
  fromJSON = const . A.Success . const ()

-- | Set addresses so that developers can verify their forms implementation.

-- | Parameters of the 'Autofill.setAddresses' command.
data PAutofillSetAddresses = PAutofillSetAddresses
  {
    pAutofillSetAddressesAddresses :: [AutofillAddress]
  }
  deriving (Eq, Show)
pAutofillSetAddresses
  :: [AutofillAddress]
  -> PAutofillSetAddresses
pAutofillSetAddresses
  arg_pAutofillSetAddressesAddresses
  = PAutofillSetAddresses
    arg_pAutofillSetAddressesAddresses
instance ToJSON PAutofillSetAddresses where
  toJSON p = A.object $ catMaybes [
    ("addresses" A..=) <$> Just (pAutofillSetAddressesAddresses p)
    ]
instance Command PAutofillSetAddresses where
  type CommandResponse PAutofillSetAddresses = ()
  commandName _ = "Autofill.setAddresses"
  fromJSON = const . A.Success . const ()

-- | Disables autofill domain notifications.

-- | Parameters of the 'Autofill.disable' command.
data PAutofillDisable = PAutofillDisable
  deriving (Eq, Show)
pAutofillDisable
  :: PAutofillDisable
pAutofillDisable
  = PAutofillDisable
instance ToJSON PAutofillDisable where
  toJSON _ = A.Null
instance Command PAutofillDisable where
  type CommandResponse PAutofillDisable = ()
  commandName _ = "Autofill.disable"
  fromJSON = const . A.Success . const ()

-- | Enables autofill domain notifications.

-- | Parameters of the 'Autofill.enable' command.
data PAutofillEnable = PAutofillEnable
  deriving (Eq, Show)
pAutofillEnable
  :: PAutofillEnable
pAutofillEnable
  = PAutofillEnable
instance ToJSON PAutofillEnable where
  toJSON _ = A.Null
instance Command PAutofillEnable where
  type CommandResponse PAutofillEnable = ()
  commandName _ = "Autofill.enable"
  fromJSON = const . A.Success . const ()

