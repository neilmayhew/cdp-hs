{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= FileSystem

-}


module CDP.Domains.FileSystem (module CDP.Domains.FileSystem) where

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
import CDP.Domains.Storage as Storage


-- | Type 'FileSystem.File'.
data FileSystemFile = FileSystemFile
  {
    fileSystemFileName :: T.Text,
    -- | Timestamp
    fileSystemFileLastModified :: DOMNetworkEmulationPageSecurity.NetworkTimeSinceEpoch,
    -- | Size in bytes
    fileSystemFileSize :: Double,
    fileSystemFileType :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON FileSystemFile where
  parseJSON = A.withObject "FileSystemFile" $ \o -> FileSystemFile
    <$> o A..: "name"
    <*> o A..: "lastModified"
    <*> o A..: "size"
    <*> o A..: "type"
instance ToJSON FileSystemFile where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (fileSystemFileName p),
    ("lastModified" A..=) <$> Just (fileSystemFileLastModified p),
    ("size" A..=) <$> Just (fileSystemFileSize p),
    ("type" A..=) <$> Just (fileSystemFileType p)
    ]

-- | Type 'FileSystem.Directory'.
data FileSystemDirectory = FileSystemDirectory
  {
    fileSystemDirectoryName :: T.Text,
    fileSystemDirectoryNestedDirectories :: [T.Text],
    -- | Files that are directly nested under this directory.
    fileSystemDirectoryNestedFiles :: [FileSystemFile]
  }
  deriving (Eq, Show)
instance FromJSON FileSystemDirectory where
  parseJSON = A.withObject "FileSystemDirectory" $ \o -> FileSystemDirectory
    <$> o A..: "name"
    <*> o A..: "nestedDirectories"
    <*> o A..: "nestedFiles"
instance ToJSON FileSystemDirectory where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (fileSystemDirectoryName p),
    ("nestedDirectories" A..=) <$> Just (fileSystemDirectoryNestedDirectories p),
    ("nestedFiles" A..=) <$> Just (fileSystemDirectoryNestedFiles p)
    ]

-- | Type 'FileSystem.BucketFileSystemLocator'.
data FileSystemBucketFileSystemLocator = FileSystemBucketFileSystemLocator
  {
    -- | Storage key
    fileSystemBucketFileSystemLocatorStorageKey :: Storage.StorageSerializedStorageKey,
    -- | Bucket name. Not passing a `bucketName` will retrieve the default Bucket. (https://developer.mozilla.org/en-US/docs/Web/API/Storage_API#storage_buckets)
    fileSystemBucketFileSystemLocatorBucketName :: Maybe T.Text,
    -- | Path to the directory using each path component as an array item.
    fileSystemBucketFileSystemLocatorPathComponents :: [T.Text]
  }
  deriving (Eq, Show)
instance FromJSON FileSystemBucketFileSystemLocator where
  parseJSON = A.withObject "FileSystemBucketFileSystemLocator" $ \o -> FileSystemBucketFileSystemLocator
    <$> o A..: "storageKey"
    <*> o A..:? "bucketName"
    <*> o A..: "pathComponents"
instance ToJSON FileSystemBucketFileSystemLocator where
  toJSON p = A.object $ catMaybes [
    ("storageKey" A..=) <$> Just (fileSystemBucketFileSystemLocatorStorageKey p),
    ("bucketName" A..=) <$> (fileSystemBucketFileSystemLocatorBucketName p),
    ("pathComponents" A..=) <$> Just (fileSystemBucketFileSystemLocatorPathComponents p)
    ]


-- | Parameters of the 'FileSystem.getDirectory' command.
data PFileSystemGetDirectory = PFileSystemGetDirectory
  {
    pFileSystemGetDirectoryBucketFileSystemLocator :: FileSystemBucketFileSystemLocator
  }
  deriving (Eq, Show)
pFileSystemGetDirectory
  :: FileSystemBucketFileSystemLocator
  -> PFileSystemGetDirectory
pFileSystemGetDirectory
  arg_pFileSystemGetDirectoryBucketFileSystemLocator
  = PFileSystemGetDirectory
    arg_pFileSystemGetDirectoryBucketFileSystemLocator
instance ToJSON PFileSystemGetDirectory where
  toJSON p = A.object $ catMaybes [
    ("bucketFileSystemLocator" A..=) <$> Just (pFileSystemGetDirectoryBucketFileSystemLocator p)
    ]
data FileSystemGetDirectory = FileSystemGetDirectory
  {
    -- | Returns the directory object at the path.
    fileSystemGetDirectoryDirectory :: FileSystemDirectory
  }
  deriving (Eq, Show)
instance FromJSON FileSystemGetDirectory where
  parseJSON = A.withObject "FileSystemGetDirectory" $ \o -> FileSystemGetDirectory
    <$> o A..: "directory"
instance Command PFileSystemGetDirectory where
  type CommandResponse PFileSystemGetDirectory = FileSystemGetDirectory
  commandName _ = "FileSystem.getDirectory"

