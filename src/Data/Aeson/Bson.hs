{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aeson.Bson (
  toAeson, aesonifyValue,
  toBson, bsonifyValue
) where

import Data.Bson as BSON
import Data.ByteString.Base64.Type (makeByteString64)
import Data.Aeson.Types as AESON
import qualified Data.Scientific as Scientific
import Data.Text as T hiding (map)
import Data.HashMap.Strict as Map (fromList, toList)
import Data.Vector as Vector (toList)
import Numeric

instance ToJSON BSON.Value where
  toJSON = aesonifyValue

-- Encoding and decoding numbers is tricky because Scientific (which
-- is used by Aeson) is capable of representing many more types of
-- numbers than Bson. Here, we encode the number as an Int64 if
-- possible and otherwise encode it as a Double (which Bson calls
-- Float). One consequence is that integers with many digits will be
-- encoded as Doubles. After decoding, some trailing digits will be
-- lost.
bsonNumberFromScientific :: Scientific.Scientific -> BSON.Value
bsonNumberFromScientific n =
  case Scientific.toBoundedInteger n of
    Just i  -> Int64 i
    Nothing -> Float (Scientific.toRealFloat n)

bsonifyValue :: AESON.Value -> BSON.Value
bsonifyValue (Object obj) = Doc $ toBson obj
bsonifyValue (AESON.Array array) = BSON.Array . map bsonifyValue . Vector.toList $ array
bsonifyValue (AESON.String str) = BSON.String str
bsonifyValue (Number n) = bsonNumberFromScientific n
bsonifyValue (AESON.Bool b) = BSON.Bool b
bsonifyValue (AESON.Null) = BSON.Null

aesonifyValue :: BSON.Value -> AESON.Value
aesonifyValue (Float f) = toJSON f
aesonifyValue (BSON.String s) = toJSON s
aesonifyValue (Doc doc) = Object (toAeson doc)
aesonifyValue (BSON.Array list) = toJSON list
aesonifyValue (Bin (Binary binary)) = toJSON (makeByteString64 binary)
aesonifyValue (Fun (Function function)) = toJSON (makeByteString64 function)
aesonifyValue (Uuid (UUID uuid)) = toJSON (makeByteString64 uuid)
aesonifyValue (Md5 (MD5 md5)) = toJSON (makeByteString64 md5)
aesonifyValue (UserDef (UserDefined userdef)) = toJSON (makeByteString64 userdef)
aesonifyValue (ObjId (Oid w32 w64)) = toJSON $ showHex w32 (showHex w64 "")
aesonifyValue (BSON.Bool bool) = toJSON bool
aesonifyValue (UTC utc) = toJSON utc
aesonifyValue (BSON.Null) = AESON.Null
aesonifyValue (RegEx (Regex pattern mods)) = toJSON $
                                           '/' : T.unpack pattern ++
                                           '/' : T.unpack mods
aesonifyValue (JavaScr (Javascript env code)) = toJSON . Map.fromList $
                                              [ (T.pack "environment", Object (toAeson env))
                                              , (T.pack "code", toJSON code)]
aesonifyValue (Sym (Symbol sym)) = toJSON sym
aesonifyValue (Int32 int32) = toJSON int32
aesonifyValue (Int64 int64) = toJSON int64
aesonifyValue (Stamp (MongoStamp int64)) = toJSON int64
aesonifyValue (MinMax mm) = case mm of { MinKey -> toJSON (-1 :: Int)
                                       ; MaxKey -> toJSON (1 :: Int)}


toBson :: AESON.Object -> BSON.Document
toBson = map (\(t, v) -> (t := bsonifyValue v)) . Map.toList

toAeson :: BSON.Document -> AESON.Object
toAeson = Map.fromList . map (\(l := v) -> (l, aesonifyValue v))
