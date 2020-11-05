{-# language DuplicateRecordFields #-}
{-# language NamedFieldPuns #-}

module Cef.Extension.Acos
  ( Field(..)
  , interpret
  ) where

import Data.Bytes (Bytes)
import Data.Word (Word8,Word16,Word64)
import Net.Types (IP)
import Cef (Pair(Pair)) 
import Data.Primitive (SmallArray)

import qualified Cef
import qualified Cef.Acos.Patterns as Patterns
import qualified Data.Primitive.Contiguous as C
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Parser as Parser
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Net.IP as IP

data Field
  = Count {-# UNPACK #-} !Word64
  | SourceIp {-# UNPACK #-} !IP
  | SourcePort {-# UNPACK #-} !Word16
  | DestinationIp {-# UNPACK #-} !IP
  | DestinationPort {-# UNPACK #-} !Word16
  | Action {-# UNPACK #-} !Bytes
  | Message {-# UNPACK #-} !Bytes
  | Custom
      {-# UNPACK #-} !Word8 -- ^ Custom field number
      {-# UNPACK #-} !Bytes -- ^ Custom field value
  | CustomLabel
      {-# UNPACK #-} !Word8 -- ^ Custom field label number
      {-# UNPACK #-} !Bytes -- ^ Custom field label value
  deriving (Eq,Show)

-- | Decode a CEF extension, interpreting fields according to ACOS\'s CEF
-- extension. This never fails. Noncompliant fields are ignored.
interpret :: SmallArray Pair -> SmallArray Field
interpret xs = C.mapMaybe decodeOne xs

decodeOne :: Pair -> Maybe Field
decodeOne Pair{key,value}
  | Patterns.isCnt key = Count <$> decodeWord64 value
  | Patterns.isSrc key = SourceIp <$> decodeIp value
  | Patterns.isDst key = DestinationIp <$> decodeIp value
  | Patterns.isSpt key = SourcePort <$> decodeWord16 value
  | Patterns.isDpt key = DestinationPort <$> decodeWord16 value
  | Patterns.isAct key = Just $! Action value
  | Patterns.isMsg key = Just $! Message value
  | Patterns.isCs1 key = Just $! Custom 1 value
  | Patterns.isCs2 key = Just $! Custom 2 value
  | Patterns.isCs3 key = Just $! Custom 3 value
  | Patterns.isCs1Label key = Just $! CustomLabel 1 value
  | Patterns.isCs2Label key = Just $! CustomLabel 2 value
  | Patterns.isCs3Label key = Just $! CustomLabel 3 value
  | otherwise = Nothing

decodeWord64 :: Bytes -> Maybe Word64
decodeWord64 = Parser.parseBytesMaybe (Latin.decWord64 ())

decodeWord16 :: Bytes -> Maybe Word16
decodeWord16 = Parser.parseBytesMaybe (Latin.decWord16 ())

decodeIp :: Bytes -> Maybe IP
decodeIp = Parser.parseBytesMaybe (IP.parserUtf8Bytes ())
