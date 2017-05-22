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

toI32 :: Integral a => a -> Int32
toI32 = fromIntegral

validateFrameType :: Maybe FrameType -> Get FrameType
validateFrameType Nothing = fail "Invalid FrameType"
validateFrameType (Just x) = return x

instance Serialize FrameHeader where
  put (FrameHeader sid t fs) =
    putInt32be (toI32 sid) >> putWord8 ((typeWord `shiftL` 2) .|. (flag8s !! 1)) >>
    putWord8 (flag8s !! 0)
    where
      typeWord = (frameType Map.! t) :: Word8
      flags = setFrameFlag fs 0 :: Word16
      flag8s =
        map fromIntegral [flags .&. 0xFF, (flags .&. 0xFF00) `shiftR` 8] :: [Word8]
  get = do
    id <- validateStreamId <$> getInt32be
    if (isNothing id)
      then fail "Invalid Stream ID"
      else do
        first <- getWord8
        second <- getWord8
        frametype <-
              validateFrameType $ (Map.lookup (first `shiftR` 2) frameType')
        let firstDec = (fromIntegral (first .&. 0x3) :: Word16) `shiftL` 8
            flags = (firstDec .|. (fromIntegral second))
            parsedFlags = filterFlags frametype $ allFrameFlags flags
        return $ FrameHeader (fromJust id) frametype parsedFlags

resumeIdTokenFramingLength flags token =
  case (elem ResumeEnableFlag flags) of
    True -> (sizeOf (undefined :: Word16)) + (sizeOf token)
    False -> 0


putMetadataWithLength Nothing = putWord16be 0 >> putWord8 0
putMetadataWithLength (Just (Metadata m))
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
putMetadataWithoutLength (Just (Metadata m)) = putByteString m

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
  put _ = fail "Not implemented"
  get = fmap Payload $ remaining >>= getByteString

putPayload Nothing = flush
putPayload (Just (Payload p)) = putByteString p

instance Serialize ErrorCode where
  put e = putWord32be $ errorCodeMap Map.! e 
  get = (errorCodeMap' Map.!) <$> getWord32be

instance Serialize ProtocolVersion where
  put e =
    putWord16be (view (protocolVersionMajor) e) >>
    putWord16be (view (protocolVersionMinor) e)
  get = ProtocolVersion <$> getWord16be <*> getWord16be

-- FIXME bounds checks mimetype lengths
putSetupParameters header (SetupParameters version keepalive lifetime (ResumeIdentificationToken token) metaMime dataMime) =
  put version >>
  putInt32be (toI32 keepalive) >>
  putInt32be (toI32 lifetime) >>
  (if (elem ResumeEnableFlag $ view headerFlags header)
     then putWord8 (fromIntegral $ length token) >> put token
     else mempty) >>
  (putWord8 $ fromIntegral $ length metaMime) >> mapM_ put metaMime >>
  (putWord8 $ fromIntegral $ length dataMime) >> mapM_ put metaMime

putMetadata header metadata = 
    if (elem MetadataFlag $ view headerFlags header)
       then putMetadataWithLength metadata
       else mempty
  
instance Serialize Frame where
  put (FrameSetup header properties metadata payload) =
    put header >> (putSetupParameters header properties) >>
    putMetadataWithLength metadata >>
    putPayload payload
  put (FrameLease header ttl reqNum metadata) =
    put header >> putInt32be (toI32 ttl) >> putInt32be (toI32 ttl) >>
    putMetadataWithoutLength metadata
  put (FrameKeepAlive header position payload) =
    put header >> putInt64be position >> putPayload payload
  put (FrameRequestResponse header metadata payload) =
    put header >> putMetadata header metadata >> putPayload payload
  put (FrameRequestFNF header metadata payload) =
    put header >> putMetadata header metadata >> putPayload payload
  put (FrameRequestStream header requestN metadata payload) =
    put header >> putInt32be requestN >> putMetadata header metadata >>
    putPayload payload
  put (FrameRequestChannel header requestN metadata payload) =
    put header >> putInt32be requestN >> putMetadata header metadata >>
    putPayload payload
  put (FrameRequestN header requestN) = put header >> putInt32be requestN
  put (FrameCancel header) = put header
  put (FramePayload header metadata payload) =
    put header >> putMetadata header metadata >> putPayload payload
  put (FrameError header errorCode errorData) =
    put header >> putWord32be (errorCodeMap Map.! errorCode) >>
    putPayload errorData
  put (FrameMetadataPush header metadata) =
    put header >> putMetadata header metadata
  put (FrameResume header version (ResumeIdentificationToken token) server client) =
    put header >> put version >> (putWord8 $ fromIntegral $ length token) >>
    (mapM_ putWord8 token) >>
    putInt64be server >>
    putInt64be client
  put (FrameResumeOK header position) = put header >> putInt64be position
  get = (get :: Get FrameHeader) >>= go
    where
      go h@(FrameHeader _ ErrorType flags) = FrameError h <$> code <*> payload
        where
          code =
            ((`Map.lookup` errorCodeMap') <$> getWord32be) >>= \x ->
              case x of
                Nothing -> fail "Invalid error"
                Just c -> return c
          payload =
            remaining >>= \x ->
              case x of
                0 -> return Nothing
                _ -> Just <$> Payload <$> getByteString x
      go _ = fail "not implemented"
{-
  do
    streamIdM <- validateStreamId <$ getInt32be
    case streamIdM of
      Nothing -> fail "Invalid stream id."
      Just streamId -> do
        w2 <- getWord8
        flagWord <-
          (((fromIntegral $ (w2 .&. 0x3) :: Word16) `shiftL` 8) .&.) <$>
          fromIntegral <Text$>
          getWord8
        let frameTypeM = Map.lookup (w2 `shiftR` 2) frameType'
            flags = allFrameFlags (w2 .&. 0x3)
        case frameTypeM of
          Nothing -> fail "Unknown frame type."
          (Just frameType) -> getFrame (FrameHeader frameType flags streamId)
-}

getFrame :: FrameHeader -> Get Frame
getFrame _ = fail "test"
