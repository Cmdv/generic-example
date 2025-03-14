{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Models where

import Data.ByteString (ByteString)
import Data.Proxy
import Data.Text (Text, unpack)
import Data.Word (Word64)
import GHC.Generics
import Quiet (Quiet (..))
import Types
import qualified Data.List.NonEmpty as NE
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Data.Int (Int64)
import Data.Functor.Contravariant ((>$<))
import Contravariant.Extras (contrazip4)

-- Simple ID types
newtype TxMetadataId = TxMetadataId { getTxMetadataId :: Int64 } deriving (Show, Eq)

newtype TxId = TxId { getTxId :: Int64 } deriving (Eq, Show, Ord)

newtype DbWord64 = DbWord64 {unDbWord64 :: Word64}
  deriving (Eq, Generic)
  deriving (Read, Show) via (Quiet DbWord64)

-- Example entity
data TxMetadata = TxMetadata
  { txMetadataKey :: !DbWord64           -- sqltype=word64type
  , txMetadataJson :: !(Maybe Text)        -- sqltype=jsonb
  , txMetadataBytes :: !ByteString       -- sqltype=bytea
  , txMetadataTxId :: !TxId              -- noreference
  } deriving (Eq, Show, Generic)

-- Create instance
instance DbInfo TxMetadata

type instance Key TxMetadata = TxMetadataId

entityTxMetadataDecoder :: D.Row (Entity TxMetadata)
entityTxMetadataDecoder =
  Entity
    <$> idDecoder TxMetadataId
    <*> txMetadataDecoder

txMetadataDecoder :: D.Row TxMetadata
txMetadataDecoder =
  TxMetadata
    <$> D.column (D.nonNullable $ DbWord64 . fromIntegral <$> D.int8) -- txMetadataKey
    <*> D.column (D.nullable D.text) -- txMetadataJson
    <*> D.column (D.nonNullable D.bytea) -- txMetadataBytes
    <*> idDecoder TxId -- txMetadataTxId

entityTxMetadataEncoder :: E.Params (Entity TxMetadata)
entityTxMetadataEncoder =
  mconcat
    [ entityKey >$< idEncoder getTxMetadataId
    , entityVal >$< txMetadataEncoder
    ]

txMetadataEncoder :: E.Params TxMetadata
txMetadataEncoder =
  mconcat
    [ txMetadataKey >$< E.param (E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    , txMetadataJson >$< E.param (E.nullable E.text)
    , txMetadataBytes >$< E.param (E.nonNullable E.bytea)
    , txMetadataTxId >$< idEncoder getTxId
    ]

txMetadataBulkEncoder :: E.Params ([DbWord64], [Maybe Text], [ByteString], [TxId])
txMetadataBulkEncoder =
  contrazip4
    (manyEncoder $ E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    (manyEncoder $ E.nullable E.text)
    (manyEncoder $ E.nonNullable E.bytea)
    (manyEncoder $ E.nonNullable $ getTxId >$< E.int8)

-- --------------------Info:--------------------
-- Table: "tx_metadata"
-- Columns:, "id", "key", "json", "bytes", "tx_id"

{-|
  Helper function to create a decoder for an id column.
  The function takes a function that constructs the id type from an Int64.
-}
idDecoder :: (Int64 -> a) -> D.Row a
idDecoder f = D.column (D.nonNullable $ f <$> D.int8)

maybeIdDecoder :: (Int64 -> a) -> D.Row (Maybe a)
maybeIdDecoder f = D.column (D.nullable $ f <$> D.int8)

{-|
  Helper function to create an encoder for an id column.
  The function takes a function that extracts the Int64 from the id type.
-}
idEncoder :: (a -> Int64) -> E.Params a
idEncoder f = E.param $ E.nonNullable $ f >$< E.int8

idBulkEncoder :: (a -> Int64) -> E.NullableOrNot E.Value  a
idBulkEncoder f = E.nonNullable $ f >$< E.int8

maybeIdEncoder :: (a -> Int64) -> E.Params (Maybe a)
maybeIdEncoder f = E.param $ E.nullable $ f >$< E.int8


-- | Creates a parameter encoder for an array of values from a single-value encoder
manyEncoder :: E.NullableOrNot E.Value a -> E.Params [a]
manyEncoder v = E.param $ E.nonNullable $ E.foldableArray v
