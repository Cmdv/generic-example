{-# LANGUAGE TypeApplications #-}
module Main where
import Models (TxMetadata)
import Types (DbInfo(..))
import Data.Data (Proxy(..))
import Data.Text (unpack)
import qualified Data.List.NonEmpty as NE

main :: IO ()
main = showTxMetadataInfo

-- Function to test
showTxMetadataInfo :: IO ()
showTxMetadataInfo = do
  putStrLn "--------------------Info:--------------------"
  putStrLn $ "Table: " ++ show (tableName (Proxy @TxMetadata))
  putStrLn "Unique fields: " <> print (uniqueFields (Proxy @TxMetadata))
  putStrLn $ "Columns:" <> concatMap ( (", " <>) . show . unpack) (NE.toList $ columnNames (Proxy @TxMetadata))
