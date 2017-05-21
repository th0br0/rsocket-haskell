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

putMetadataWithLength Nothing = putWord16be 0 >> putWord8 0
putMetadataWithLength (Just m)
  | (B.null m) = putWord16be 0 >> putWord8 0
  | (B.length m) < (2 ^ 21) = fail "Metadata is longer than 2**21-1"
  | otherwise = do
    let length = fromIntegral (B.length m) :: Word32
        lengthEnc :: [Word8]
        lengthEnc =
          map
            fromIntegral
            [length `shiftR` 16, (length `shiftR` 8) .&. 0xFF, length .&. 0xFF]
    putWord8 (lengthEnc !! 0) >> putWord8 (lengthEnc !! 1) >>
      putWord8 (lengthEnc !! 2) >>
      putByteString m
-- FIXME identity?
putMetadataWithoutLength Nothing = flush
putMetadataWithoutLength (Just m) = putByteString m

getMetadataWithoutLength :: Get (Maybe Metadata)
getMetadataWithoutLength = do
  available <- remaining

  if (available == 0) then return Nothing else do
    d <- getByteString available
    return $ Just $ Metadata d
getMetadataWithLength :: Get (Maybe Metadata)
getMetadataWithLength = do
  w1 <- fromIntegral <$> getWord8 :: Get Word32
  w2 <- fromIntegral <$> getWord8 :: Get Word32
  w3 <- fromIntegral <$> getWord8 :: Get Word32

  let length = fromIntegral $ ((w1 `shiftL` 16) .|. (w2 `shiftL` 8) .|. w3) :: Int

  if (length == 0) then return Nothing else do
    d <- getByteString length 
    return $ Just $ Metadata d

instance Serialize Payload where
  put (Payload d) = put d
  get = fmap Payload $ remaining >>= getByteString

instance Serialize ErrorCode where
  put e = putWord32be $ errorCodeMap Map.! e 
  get = fmap (errorCodeMap' Map.!) getWord32be

instance Serialize Frame where
  put (FrameError header code payload) = put header >> put code >> put payload
  get = fail "Frame not implemented yet."
