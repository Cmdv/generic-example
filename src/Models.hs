{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Models where

import GHC.Generics
import Data.Text (Text, unpack)
import Data.Proxy
import Types
import qualified Data.List.NonEmpty as NE

-- Simple ID types
newtype TxMetadataId = TxMetadataId { getTxMetadataId :: Int } deriving (Show, Eq)

-- Example entity
data TxMetadata = TxMetadata
  { txMetadataId :: !TxMetadataId
  , txMetadataKey :: !Int
  , txMetadataJson :: !(Maybe Text)
  , txMetadataBytes :: !Int
  , txMetadataTxId :: !Int
  } deriving (Show, Eq, Generic)

-- Create instance
instance HasDbInfo TxMetadata where
  -- Column names are derived automatically

-- Function to test
showTxMetadataInfo :: IO ()
showTxMetadataInfo = do
  putStrLn "--------------------Info:--------------------"
  putStrLn $ "Table: " ++ show (tableName (Proxy @TxMetadata))
  putStrLn $ "Columns:" <> concatMap ( (", " <>) . show . unpack) (NE.toList $ columnNames (Proxy @TxMetadata))

-- --------------------Info:--------------------
-- Table: "tx_metadata"
-- Columns:, "id", "key", "json", "bytes", "tx_id"
