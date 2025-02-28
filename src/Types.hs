{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as T
import Data.Proxy
import qualified Data.List.NonEmpty as NE
import Data.Char (toLower, isUpper)
import Data.Typeable (Typeable, typeRep, typeRepTyCon, tyConName)
import Data.List (stripPrefix)

-- | Type class for types that have database information
class Typeable a => HasDbInfo a where
  tableName :: Proxy a -> Text
  default tableName :: Proxy a -> Text
  tableName = T.pack . camelToSnake . tyConName . typeRepTyCon . typeRep

  columnNames :: Proxy a -> NE.NonEmpty Text
  default columnNames :: (Generic a, GRecordFieldNames (Rep a), Typeable a) => Proxy a -> NE.NonEmpty Text
  columnNames p =
    let typeName = tyConName $ typeRepTyCon $ typeRep p
        fieldNames = gRecordFieldNames (from (undefined :: a))
    in case fieldNames of
         [] -> error "No fields found"
         ns -> NE.fromList $ map (fieldToColumnWithType typeName) ns

-- | Convert a field name to a column name
fieldToColumnWithType :: String -> String -> Text
fieldToColumnWithType typeName field = T.pack $ camelToSnake $
  case stripPrefix (uncamelize typeName) field of
    Just remaining -> case remaining of
      (c:_) | isUpper c -> remaining
      _otherwise -> error $ "Field name '" ++ field ++ "' does not match pattern '"
                  ++ uncamelize typeName ++ "X...'"
    Nothing -> error $ "Field name '" ++ field ++ "' does not start with type prefix '"
                     ++ uncamelize typeName ++ "'"
-- | Convert a string to snake case
uncamelize :: String -> String
uncamelize [] = []
uncamelize (x:xs) = toLower x : xs

-- | Convert a camel case string to snake case
camelToSnake :: String -> String
camelToSnake [] = []
camelToSnake (x:xs) = toLower x : go xs
  where
    go [] = []
    go (c:cs)
      | isUpper c = '_' : toLower c : go cs
      | otherwise = c : go cs

-- | Type class for generic representation of record field names
class GRecordFieldNames f where
  gRecordFieldNames :: f p -> [String]

instance GRecordFieldNames U1 where
  gRecordFieldNames _ = []

instance (GRecordFieldNames a, GRecordFieldNames b) => GRecordFieldNames (a :*: b) where
  gRecordFieldNames _ = gRecordFieldNames (undefined :: a p) ++ gRecordFieldNames (undefined :: b p)

instance GRecordFieldNames a => GRecordFieldNames (M1 D c a) where
  gRecordFieldNames _ = gRecordFieldNames (undefined :: a p)

instance GRecordFieldNames a => GRecordFieldNames (M1 C c a) where
  gRecordFieldNames _ = gRecordFieldNames (undefined :: a p)

instance (Selector c) => GRecordFieldNames (M1 S c (K1 i a)) where
  gRecordFieldNames m = [selName m]

instance GRecordFieldNames (K1 i c) where
  gRecordFieldNames _ = []
