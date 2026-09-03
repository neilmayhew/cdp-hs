{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= WebMCP

-}


module CDP.Domains.WebMCP (module CDP.Domains.WebMCP) where

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
import CDP.Domains.Runtime as Runtime


-- | Type 'WebMCP.Annotation'.
--   Tool annotations
data WebMCPAnnotation = WebMCPAnnotation
  {
    -- | A hint indicating that the tool does not modify any state.
    webMCPAnnotationReadOnly :: Maybe Bool,
    -- | A hint indicating that the tool output may contain untrusted content, ex: UGC, 3rd party data.
    webMCPAnnotationUntrustedContent :: Maybe Bool,
    -- | If the declarative tool was declared with the autosubmit attribute.
    webMCPAnnotationAutosubmit :: Maybe Bool
  }
  deriving (Eq, Show)
instance FromJSON WebMCPAnnotation where
  parseJSON = A.withObject "WebMCPAnnotation" $ \o -> WebMCPAnnotation
    <$> o A..:? "readOnly"
    <*> o A..:? "untrustedContent"
    <*> o A..:? "autosubmit"
instance ToJSON WebMCPAnnotation where
  toJSON p = A.object $ catMaybes [
    ("readOnly" A..=) <$> (webMCPAnnotationReadOnly p),
    ("untrustedContent" A..=) <$> (webMCPAnnotationUntrustedContent p),
    ("autosubmit" A..=) <$> (webMCPAnnotationAutosubmit p)
    ]

-- | Type 'WebMCP.InvocationStatus'.
--   Represents the status of a tool invocation.
data WebMCPInvocationStatus = WebMCPInvocationStatusCompleted | WebMCPInvocationStatusCanceled | WebMCPInvocationStatusError
  deriving (Ord, Eq, Show, Read)
instance FromJSON WebMCPInvocationStatus where
  parseJSON = A.withText "WebMCPInvocationStatus" $ \v -> case v of
    "Completed" -> pure WebMCPInvocationStatusCompleted
    "Canceled" -> pure WebMCPInvocationStatusCanceled
    "Error" -> pure WebMCPInvocationStatusError
    "_" -> fail "failed to parse WebMCPInvocationStatus"
instance ToJSON WebMCPInvocationStatus where
  toJSON v = A.String $ case v of
    WebMCPInvocationStatusCompleted -> "Completed"
    WebMCPInvocationStatusCanceled -> "Canceled"
    WebMCPInvocationStatusError -> "Error"

-- | Type 'WebMCP.Tool'.
--   Definition of a tool that can be invoked.
data WebMCPTool = WebMCPTool
  {
    -- | Tool name.
    webMCPToolName :: T.Text,
    -- | Tool description.
    webMCPToolDescription :: T.Text,
    -- | Schema for the tool's input parameters.
    webMCPToolInputSchema :: Maybe [(T.Text, T.Text)],
    -- | Optional annotations for the tool.
    webMCPToolAnnotations :: Maybe WebMCPAnnotation,
    -- | Frame identifier associated with the tool registration.
    webMCPToolFrameId :: DOMNetworkEmulationPageSecurity.PageFrameId,
    -- | Optional node ID for declarative tools.
    webMCPToolBackendNodeId :: Maybe DOMNetworkEmulationPageSecurity.DOMBackendNodeId,
    -- | The stack trace at the time of the registration.
    webMCPToolStackTrace :: Maybe Runtime.RuntimeStackTrace
  }
  deriving (Eq, Show)
instance FromJSON WebMCPTool where
  parseJSON = A.withObject "WebMCPTool" $ \o -> WebMCPTool
    <$> o A..: "name"
    <*> o A..: "description"
    <*> o A..:? "inputSchema"
    <*> o A..:? "annotations"
    <*> o A..: "frameId"
    <*> o A..:? "backendNodeId"
    <*> o A..:? "stackTrace"
instance ToJSON WebMCPTool where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (webMCPToolName p),
    ("description" A..=) <$> Just (webMCPToolDescription p),
    ("inputSchema" A..=) <$> (webMCPToolInputSchema p),
    ("annotations" A..=) <$> (webMCPToolAnnotations p),
    ("frameId" A..=) <$> Just (webMCPToolFrameId p),
    ("backendNodeId" A..=) <$> (webMCPToolBackendNodeId p),
    ("stackTrace" A..=) <$> (webMCPToolStackTrace p)
    ]

-- | Type 'WebMCP.RemovedTool'.
--   Definition of a tool that was removed.
data WebMCPRemovedTool = WebMCPRemovedTool
  {
    -- | Tool name.
    webMCPRemovedToolName :: T.Text,
    -- | Frame identifier associated with the tool registration.
    webMCPRemovedToolFrameId :: DOMNetworkEmulationPageSecurity.PageFrameId
  }
  deriving (Eq, Show)
instance FromJSON WebMCPRemovedTool where
  parseJSON = A.withObject "WebMCPRemovedTool" $ \o -> WebMCPRemovedTool
    <$> o A..: "name"
    <*> o A..: "frameId"
instance ToJSON WebMCPRemovedTool where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (webMCPRemovedToolName p),
    ("frameId" A..=) <$> Just (webMCPRemovedToolFrameId p)
    ]

-- | Type of the 'WebMCP.toolsAdded' event.
data WebMCPToolsAdded = WebMCPToolsAdded
  {
    -- | Array of tools that were added.
    webMCPToolsAddedTools :: [WebMCPTool]
  }
  deriving (Eq, Show)
instance FromJSON WebMCPToolsAdded where
  parseJSON = A.withObject "WebMCPToolsAdded" $ \o -> WebMCPToolsAdded
    <$> o A..: "tools"
instance Event WebMCPToolsAdded where
  eventName _ = "WebMCP.toolsAdded"

-- | Type of the 'WebMCP.toolsRemoved' event.
data WebMCPToolsRemoved = WebMCPToolsRemoved
  {
    -- | Array of tools that were removed.
    webMCPToolsRemovedTools :: [WebMCPRemovedTool]
  }
  deriving (Eq, Show)
instance FromJSON WebMCPToolsRemoved where
  parseJSON = A.withObject "WebMCPToolsRemoved" $ \o -> WebMCPToolsRemoved
    <$> o A..: "tools"
instance Event WebMCPToolsRemoved where
  eventName _ = "WebMCP.toolsRemoved"

-- | Type of the 'WebMCP.toolInvoked' event.
data WebMCPToolInvoked = WebMCPToolInvoked
  {
    -- | Name of the tool to invoke.
    webMCPToolInvokedToolName :: T.Text,
    -- | Frame id
    webMCPToolInvokedFrameId :: DOMNetworkEmulationPageSecurity.PageFrameId,
    -- | Invocation identifier.
    webMCPToolInvokedInvocationId :: T.Text,
    -- | The input parameters used for the invocation.
    webMCPToolInvokedInput :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON WebMCPToolInvoked where
  parseJSON = A.withObject "WebMCPToolInvoked" $ \o -> WebMCPToolInvoked
    <$> o A..: "toolName"
    <*> o A..: "frameId"
    <*> o A..: "invocationId"
    <*> o A..: "input"
instance Event WebMCPToolInvoked where
  eventName _ = "WebMCP.toolInvoked"

-- | Type of the 'WebMCP.toolResponded' event.
data WebMCPToolResponded = WebMCPToolResponded
  {
    -- | Invocation identifier.
    webMCPToolRespondedInvocationId :: T.Text,
    -- | Status of the invocation.
    webMCPToolRespondedStatus :: WebMCPInvocationStatus,
    -- | Output or error delivered as delivered to the agent. Missing if `status` is anything other than Completed.
    --   Note: The output is untrusted and poses a prompt injection risk. Clients should treat this as potentially malicious user input.
    webMCPToolRespondedOutput :: Maybe A.Value,
    -- | Error text for protocol users.
    webMCPToolRespondedErrorText :: Maybe T.Text,
    -- | The exception object, if the javascript tool threw an error>
    webMCPToolRespondedException :: Maybe Runtime.RuntimeRemoteObject
  }
  deriving (Eq, Show)
instance FromJSON WebMCPToolResponded where
  parseJSON = A.withObject "WebMCPToolResponded" $ \o -> WebMCPToolResponded
    <$> o A..: "invocationId"
    <*> o A..: "status"
    <*> o A..:? "output"
    <*> o A..:? "errorText"
    <*> o A..:? "exception"
instance Event WebMCPToolResponded where
  eventName _ = "WebMCP.toolResponded"

-- | Enables the WebMCP domain, allowing events to be sent. Enabling the domain will trigger a toolsAdded event for
--   all currently registered tools.

-- | Parameters of the 'WebMCP.enable' command.
data PWebMCPEnable = PWebMCPEnable
  deriving (Eq, Show)
pWebMCPEnable
  :: PWebMCPEnable
pWebMCPEnable
  = PWebMCPEnable
instance ToJSON PWebMCPEnable where
  toJSON _ = A.Null
instance Command PWebMCPEnable where
  type CommandResponse PWebMCPEnable = ()
  commandName _ = "WebMCP.enable"
  fromJSON = const . A.Success . const ()

-- | Disables the WebMCP domain.

-- | Parameters of the 'WebMCP.disable' command.
data PWebMCPDisable = PWebMCPDisable
  deriving (Eq, Show)
pWebMCPDisable
  :: PWebMCPDisable
pWebMCPDisable
  = PWebMCPDisable
instance ToJSON PWebMCPDisable where
  toJSON _ = A.Null
instance Command PWebMCPDisable where
  type CommandResponse PWebMCPDisable = ()
  commandName _ = "WebMCP.disable"
  fromJSON = const . A.Success . const ()

-- | Invokes a registered tool.

-- | Parameters of the 'WebMCP.invokeTool' command.
data PWebMCPInvokeTool = PWebMCPInvokeTool
  {
    -- | Frame in which to invoke the tool.
    pWebMCPInvokeToolFrameId :: DOMNetworkEmulationPageSecurity.PageFrameId,
    -- | Name of the tool to invoke.
    pWebMCPInvokeToolToolName :: T.Text,
    -- | Input parameters for the tool, matching the tool's inputSchema.
    pWebMCPInvokeToolInput :: [(T.Text, T.Text)]
  }
  deriving (Eq, Show)
pWebMCPInvokeTool
  {-
  -- | Frame in which to invoke the tool.
  -}
  :: DOMNetworkEmulationPageSecurity.PageFrameId
  {-
  -- | Name of the tool to invoke.
  -}
  -> T.Text
  {-
  -- | Input parameters for the tool, matching the tool's inputSchema.
  -}
  -> [(T.Text, T.Text)]
  -> PWebMCPInvokeTool
pWebMCPInvokeTool
  arg_pWebMCPInvokeToolFrameId
  arg_pWebMCPInvokeToolToolName
  arg_pWebMCPInvokeToolInput
  = PWebMCPInvokeTool
    arg_pWebMCPInvokeToolFrameId
    arg_pWebMCPInvokeToolToolName
    arg_pWebMCPInvokeToolInput
instance ToJSON PWebMCPInvokeTool where
  toJSON p = A.object $ catMaybes [
    ("frameId" A..=) <$> Just (pWebMCPInvokeToolFrameId p),
    ("toolName" A..=) <$> Just (pWebMCPInvokeToolToolName p),
    ("input" A..=) <$> Just (pWebMCPInvokeToolInput p)
    ]
data WebMCPInvokeTool = WebMCPInvokeTool
  {
    -- | Unique identifier for this invocation. Response is sent before tool events.
    webMCPInvokeToolInvocationId :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON WebMCPInvokeTool where
  parseJSON = A.withObject "WebMCPInvokeTool" $ \o -> WebMCPInvokeTool
    <$> o A..: "invocationId"
instance Command PWebMCPInvokeTool where
  type CommandResponse PWebMCPInvokeTool = WebMCPInvokeTool
  commandName _ = "WebMCP.invokeTool"

-- | Cancels a pending tool invocation.

-- | Parameters of the 'WebMCP.cancelInvocation' command.
data PWebMCPCancelInvocation = PWebMCPCancelInvocation
  {
    -- | Invocation identifier to cancel.
    pWebMCPCancelInvocationInvocationId :: T.Text
  }
  deriving (Eq, Show)
pWebMCPCancelInvocation
  {-
  -- | Invocation identifier to cancel.
  -}
  :: T.Text
  -> PWebMCPCancelInvocation
pWebMCPCancelInvocation
  arg_pWebMCPCancelInvocationInvocationId
  = PWebMCPCancelInvocation
    arg_pWebMCPCancelInvocationInvocationId
instance ToJSON PWebMCPCancelInvocation where
  toJSON p = A.object $ catMaybes [
    ("invocationId" A..=) <$> Just (pWebMCPCancelInvocationInvocationId p)
    ]
instance Command PWebMCPCancelInvocation where
  type CommandResponse PWebMCPCancelInvocation = ()
  commandName _ = "WebMCP.cancelInvocation"
  fromJSON = const . A.Success . const ()

