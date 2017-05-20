module RSocket.ProtoV10 where

import Data.Serialize
import Data.Serialize.Get
import Data.Serialize.Put

import Control.Lens 
import Data.Word
import Data.Int
import Data.String
import Data.Bits
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B

import RSocket.Frames

validateStreamId :: Int32 -> Maybe StreamId
validateStreamId id
  | id >= 0 = Just $ fromIntegral id
  | otherwise = Nothing

validateFrameType :: Maybe FrameType -> FrameType
validateFrameType Nothing = error "Invalid FrameType"
validateFrameType (Just x) = x

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
