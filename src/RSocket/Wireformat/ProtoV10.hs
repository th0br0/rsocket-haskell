module RSocket.Wireformat.ProtoV10 where

import Data.Serialize
import Data.Serialize.Get
import Data.Serialize.Put

import Control.Monad
import Control.Lens 
import Data.Word
import Data.Int
import Data.String
import Data.Bits
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B
import Foreign.Storable (sizeOf)

import RSocket.Frames
import RSocket.Wireformat.Validators

instance Serialize FrameHeader where
  put (FrameHeader t fs sid) =
    putInt32be (fromIntegral $ sid) >>
    putWord8 ((typeWord `shiftL` 2) .|. (flag8s !! 1)) >>
    putWord8 (flag8s !! 0)
    where
      typeWord = (frameType Map.! t) :: Word8
      flags = setFrameFlag 0 fs :: Word16
      flag8s =
        map fromIntegral [flags .&. 0xFF, (flags .&. 0xFF00) `shiftR` 8] :: [Word8]
  get = do
    id <- fmap validateStreamId getInt32be
    if (isNothing id)
      then fail "Invalid Stream ID"
      else do
        first <- getWord8
        second <- getWord8
        let frametype =
              validateFrameType $ Map.lookup (first `shiftR` 2) frameType'
            firstDec = (fromIntegral (first .&. 0x3) :: Word16) `shiftL` 8
            flags = (firstDec .|. (fromIntegral second))
            parsedFlags = filterFlags frametype $ allFrameFlags flags
        return $ FrameHeader frametype parsedFlags (fromJust id)

maxMetadataLength = 0xFFFFFF -- 24 bit max value
metadataLengthSize = 3 -- bytes
payloadFramingSize payload =
  case (view metadata payload) of
    Nothing -> 0
    otherwise -> metadataLengthSize

resumeIdTokenFramingLength flags token =
  case (elem ResumeEnableFlag flags) of
    True -> (sizeOf (undefined :: Word16)) + (sizeOf token)
    False -> 0

peekStreamId = lookAhead $ validateStreamId <$ getInt32be
peekFrameType =
  lookAhead $ do
    skip (sizeOf $ (undefined :: Word32))
    typeWord <- fmap (`shiftR` 2) getWord8
    return $ Map.lookup typeWord frameType'

instance Serialize Metadata where
  put (Metadata m)
    | (B.null m) = flush
    | (B.length m) > metadataLengthSize = fail "Metadata too big to serialize"
    | otherwise = do
      let length = fromIntegral (B.length m) :: Word32
      putWord32be length >> putByteString m
  get = do
    length <- fmap (fromIntegral :: Word32 -> Int) getWord32be
    if length > maxMetadataLength
      then fail "Metadata is too big to deserialize"
      else fmap Metadata $ getByteString length

instance Serialize Data where
  put (Data d) = put d
  get = fmap Data $ remaining >>= getByteString

instance Serialize ErrorCode where
  put e = putWord32be $ errorCodeMap Map.! e 
  get = fmap (errorCodeMap' Map.!) getWord32be

instance Serialize Payload where
  put (Payload Nothing Nothing) = flush
  put (Payload (Just p) Nothing) = put p
  put (Payload Nothing (Just m)) = put m
  put (Payload (Just p) (Just m)) = put m >> put p
  get = fail "Not implemented"

instance Serialize Frame where
  put (FrameError header code payload) = put header >> put code >> put payload
  get = fail "Frame not implemented yet."
