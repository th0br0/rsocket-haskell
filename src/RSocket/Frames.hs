{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module RSocket.Frames where

import           Data.Bits
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict      as Map
import           Data.Tuple
import           Data.Word
import           Data.String
import           Data.Int
import           Control.Lens         hiding (Empty)

type StreamId = Word32
type FrameFlags = Word16
type ResumePosition = Int64

data FrameType
  = ReservedType
  | SetupType
  | LeaseType
  | KeepAliveType
  | RequestResponseType
  | RequestFNFType
  | RequestStreamType 
  | RequestChannelType
  | RequestNType
  | CancelType
  | PayloadType
  | ErrorType
  | MetadataPushType
  | ResumeType
  | ResumeOKType
  | ExtType
  deriving (Show, Eq, Enum, Ord)

frameType =
  Map.fromList
    [ (ReservedType, 0x00)
    , (SetupType, 0x01)
    , (LeaseType, 0x02)
    , (KeepAliveType, 0x03)
    , (RequestResponseType, 0x04)
    , (RequestFNFType, 0x05)
    , (RequestStreamType, 0x06)
    , (RequestChannelType, 0x07)
    , (RequestNType, 0x08)
    , (CancelType, 0x09)
    , (PayloadType, 0x0A)
    , (ErrorType, 0x0B)
    , (MetadataPushType, 0x0C)
    , (ResumeType, 0x0D)
    , (ResumeOKType, 0x0E)
    , (ExtType, 0x3F)
    ] :: Map.Map FrameType Word8

frameType' = Map.fromList $ map swap (Map.assocs frameType)

data ErrorCode
  = ReservedError
    -- The Setup frame is invalid for the server (it could be that the client is too recent for the old server). Stream ID MUST be 0.
  | InvalidSetupError
    -- Some (or all) of the parameters specified by the client are unsupported by the server. Stream ID MUST be 0.
  | UnsupportedSetupError
    -- The server rejected the setup, it can specify the reason in the payload. Stream ID MUST be 0.
  | RejectedSetupError
    -- The connection is being terminated. Stream ID MUST be 0.
  | ConnectionError
    -- Application layer logic generating a Reactive Streams onError event. Stream ID MUST be non-0.
  | ApplicationError
    -- Despite being a valid request, the Responder decided to reject it. The Responder guarantees that it
    -- didn't process the request. The reason for the rejection is explained in the metadata section. Stream ID MUST be non-0.
  | RejectedError
    -- The responder canceled the request but has potentially started processing it
    -- (almost identical to REJECTED but doesn't garantee that no side-effects have been started). Stream ID MUST be non-0.
  | CancelledError
    -- The request is invalid. Stream ID MUST be non-0.
  | InvalidError
  deriving (Show, Eq, Enum, Ord)

errorCodeMap =
  Map.fromList
    [ (ReservedError, 0x00)
    , (InvalidSetupError, 0x0001)
    , (UnsupportedSetupError, 0x0002)
    , (RejectedSetupError, 0x0003)
    , (ConnectionError, 0x0101)
    , (ApplicationError, 0x0201)
    , (RejectedError, 0x0202)
    , (CancelledError, 0x0203)
    , (InvalidError, 0x0204)
    ] :: Map.Map ErrorCode Word32
errorCodeMap' = Map.fromList $ map swap (Map.assocs errorCodeMap)


data FrameFlag
  = EmptyFlag
  | IgnoreFlag
  | MetadataFlag
  | ResumeEnableFlag
  | LeaseFlag
  | KeepAliveRespondFlag
  | FollowsFlag
  | CompleteFlag
  | NextFlag
  deriving (Show, Eq, Enum, Ord)

frameFlagMap =
  Map.fromList
    [ (EmptyFlag, 0)
    , (IgnoreFlag, 9)
    , (MetadataFlag, 8)
    -- Setup
    , (ResumeEnableFlag, 7)
    , (LeaseFlag, 6)
    -- KeepAlive
    , (KeepAliveRespondFlag, 7)
    -- RequestResponse, RequestFnf, RequestStream, RequestChannel, Payload
    , (FollowsFlag, 7)
    -- RequestChannel, Payload
    , (CompleteFlag, 6)
    -- Payload
    , (NextFlag, 5)
    ] :: Map.Map FrameFlag Int

setFrameFlag :: Bits a => a -> [FrameFlag] -> a
setFrameFlag v []     = v
setFrameFlag v (f:fs) = setFrameFlag (setBit v (frameFlagMap Map.! f)) fs

isFrameFlag :: Bits a => a -> FrameFlag -> Bool
isFrameFlag v f = testBit v (frameFlagMap Map.! f)

allFrameFlags :: Bits a => a -> [FrameFlag]
allFrameFlags a = Map.foldlWithKey' testFlag [] frameFlagMap
  where testFlag xs k v
          | isFrameFlag a k = k : xs
          | otherwise = xs

declareClassy
  [d|

  data Payload = Payload{payloadFlags :: ![FrameFlag],
                         content :: BS.ByteString, metadata :: BS.ByteString}
               deriving (Eq, Show)

  data ResumeIdentificationToken = ResumeIdentificationToken [Word8]
                                 deriving (Eq, Show, Ord)

  data FrameHeader = FrameHeader{headerType :: !FrameType,
                                 headerFlags :: ![FrameFlag], headerStreamId :: !StreamId}
                   deriving (Eq, Show)

  data FrameRequestBase = FrameRequestBase{requestN :: !Word32,
                                           _reqBasePayload :: Payload}
                        deriving (Eq, Show)
  |]

instance HasPayload FrameRequestBase where
  payload = _reqBasePayload

declareLenses
  [d|

  data ProtocolVersion = ProtocolVersion{protocolVersionMajor ::
                                         Word16,
                                         protocolVersionMinor :: Word16}
                       deriving (Eq, Show, Ord)

  data FrameRequestN = FrameRequestN{n :: !Word32,
                                     _reqNframeHeader :: !FrameHeader}
                     deriving (Eq, Show)

  data FrameRequestStream = FrameRequestStream{_reqStreamHeader ::
                                               !FrameHeader,
                                               _reqStreamBase :: !FrameRequestBase}
                          deriving (Eq, Show)

  data FrameRequestChannel = FrameRequestChannel{_reqChannelHeader ::
                                                 !FrameHeader,
                                                 _reqChannelBase :: !FrameRequestBase}
                           deriving (Eq, Show)

  data FrameRequestResponse = FrameRequestResponse{_reqResponseHeader
                                                   :: !FrameHeader,
                                                   _reqResponseBase :: !FrameRequestBase}
                            deriving (Eq, Show)

  data FrameRequestFNF = FrameRequestFNF{_reqFNFHeader ::
                                         !FrameHeader,
                                         _reqFNFBase :: !FrameRequestBase}
                       deriving (Eq, Show)

  data FrameMetadataPush = FrameMetadataPush{_metadataPushHeader ::
                                             !FrameHeader,
                                             _metadataPayload :: Payload}
                         deriving (Eq, Show)

  data FrameCancel = FrameCancel{_cancelHeader :: !FrameHeader}
                   deriving (Eq, Show)

  data FramePayload = FramePayload{_payloadHeader :: !FrameHeader,
                                   _payloadPayload :: Payload}
                    deriving (Eq, Show)

  data FrameError = FrameError{_errorHeader :: !FrameHeader,
                               errorCode :: !ErrorCode, _errorPayload :: Payload}
                  deriving (Eq, Show)

  data FrameKeepAlive = FrameKeepAlive{_keepAliveHeader ::
                                       !FrameHeader,
                                       resumePosition :: !ResumePosition,
                                       _keepAlivePayload :: Payload}
                      deriving (Eq, Show)

  data FrameSetup = FrameSetup{_setupHeader :: !FrameHeader,
                               versionMajor :: !Word16, versionMinor :: !Word16,
                               keepaliveTime :: !Word32, maxLifetime :: !Word32,
                               _setupResumeToken :: !ResumeIdentificationToken,
                               metadataMimeType :: !String, dataMimeType :: !String,
                               _setupPayload :: Payload}
                  deriving (Eq, Show)

  data FrameLease = FrameLease{_leaseHeader :: !FrameHeader,
                               ttl :: Word32, numberOfRequests :: Word32,
                               leaseMetadata :: Maybe BS.ByteString}
                  deriving (Eq, Show)

  data FrameResume = FrameResume{_resumeHeader :: !FrameHeader,
                                 _resumeResumeToken :: !ResumeIdentificationToken,
                                 lastReceivedServerPosition :: !ResumePosition,
                                 clientPosition :: !ResumePosition,
                                 protocolVersion :: !ProtocolVersion}
                   deriving (Eq, Show)

  data FrameResumeOK = FrameResumeOK{_resumeOKHeader :: !FrameHeader,
                                     position :: !ResumePosition}
                     deriving (Eq, Show)
  |]

instance HasFrameHeader FrameRequestN where
  frameHeader = _reqNframeHeader

instance HasFrameHeader FrameRequestStream where
  frameHeader = _reqStreamHeader
instance HasFrameRequestBase FrameRequestStream where
  frameRequestBase = _reqStreamBase

instance HasFrameHeader FrameRequestChannel where
  frameHeader = _reqChannelHeader
instance HasFrameRequestBase FrameRequestChannel where
  frameRequestBase = _reqChannelBase

instance HasFrameHeader FrameRequestResponse where
  frameHeader = _reqResponseHeader
instance HasFrameRequestBase FrameRequestResponse  where
  frameRequestBase = _reqResponseBase

instance HasFrameHeader FrameRequestFNF where
  frameHeader = _reqFNFHeader
instance HasFrameRequestBase FrameRequestFNF where
  frameRequestBase = _reqFNFBase

instance HasFrameHeader FrameMetadataPush where
  frameHeader = _metadataPushHeader
instance HasPayload FrameMetadataPush where
  payload = _metadataPayload

instance HasFrameHeader FrameCancel where
  frameHeader = _cancelHeader

instance HasFrameHeader FramePayload where
  frameHeader = _payloadHeader
instance HasPayload FramePayload where
  payload = _payloadPayload

instance HasFrameHeader FrameError where
  frameHeader = _errorHeader
instance HasPayload FrameError where
  payload = _errorPayload

instance HasFrameHeader FrameKeepAlive where
  frameHeader = _keepAliveHeader
instance HasPayload FrameKeepAlive where
  payload = _keepAlivePayload

instance HasFrameHeader FrameSetup where
  frameHeader = _setupHeader
instance HasPayload FrameSetup where
  payload = _setupPayload
instance HasResumeIdentificationToken FrameSetup where
  resumeIdentificationToken = _setupResumeToken

instance HasFrameHeader FrameLease where
  frameHeader = _leaseHeader

instance HasFrameHeader FrameResume where
  frameHeader = _resumeHeader
instance HasResumeIdentificationToken FrameResume where
  resumeIdentificationToken = _resumeResumeToken

instance HasFrameHeader FrameResumeOK where
  frameHeader = _resumeOKHeader

allowedFlags :: FrameType -> [FrameFlag]
allowedFlags t
  | elem t [RequestStreamType, RequestResponseType, RequestStreamType] =
    [MetadataFlag, FollowsFlag]
  | t == RequestChannelType = [MetadataFlag, FollowsFlag, CompleteFlag]
  | t == PayloadType = [MetadataFlag, FollowsFlag, CompleteFlag, NextFlag]
  | t == ErrorType || t == LeaseType = [MetadataFlag]
  | t == KeepAliveType = [KeepAliveRespondFlag]
  | t == SetupType = [MetadataFlag, ResumeEnableFlag, LeaseFlag]
  | otherwise = [EmptyFlag]

filterFlags :: FrameType -> [FrameFlag] -> [FrameFlag]
filterFlags _ [] = [EmptyFlag]
filterFlags t fs = filter (\x -> elem x $ allowedFlags t) fs

newFrameRequestN streamId n = FrameRequestN n (FrameHeader RequestNType [EmptyFlag] streamId)
newFrameRequestStream streamId flags n payload =
  FrameRequestStream
    (FrameHeader
       RequestStreamType
       (filterFlags RequestStreamType (flags ++ (view payloadFlags payload)))
       streamId)
    (FrameRequestBase n payload)
newFrameRequestChannel streamId flags n payload =
  FrameRequestChannel
    (FrameHeader
       RequestChannelType
       (filterFlags RequestChannelType (flags ++ (view payloadFlags payload)))
       streamId)
    (FrameRequestBase n payload)
newFrameRequestResponse streamId flags n payload =
  FrameRequestResponse
    (FrameHeader
       RequestResponseType
       (filterFlags RequestResponseType (flags ++ (view payloadFlags payload)))
       streamId)
    (FrameRequestBase n payload)
newFrameRequestFNF streamId flags n payload =
  FrameRequestFNF
    (FrameHeader
       RequestFNFType
       (filterFlags RequestFNFType (flags ++ (view payloadFlags payload)))
       streamId)
    (FrameRequestBase n payload)

newFrameMetadataPush :: BS.ByteString -> FrameMetadataPush
newFrameMetadataPush m =
  FrameMetadataPush
    (FrameHeader MetadataPushType [MetadataFlag] 0)
    (Payload [] BS.empty m)

newFrameCancel streamId = FrameCancel (FrameHeader CancelType [] streamId)
newFramePayload streamId payload =
  FramePayload
    (FrameHeader
       PayloadType
       (filterFlags PayloadType (view payloadFlags payload))
       streamId)
    payload
newFrameError streamId errorCode payload =
  FrameError
    (FrameHeader
       ErrorType
       (filterFlags ErrorType (view payloadFlags payload))
       streamId)
    errorCode
    payload
newFrameKeepAlive flags position metadata =
  FrameKeepAlive
    (FrameHeader KeepAliveType (filterFlags KeepAliveType flags) 0)
    position
    (Payload [] BS.empty metadata)

-- FIXME maxKeepaliveTime maxLifetime and >0 bound checks
newFrameSetup flags versionMajor versionMinor keepaliveTime maxLifeTime token metadataMimeType dataMimeType payload =
  FrameSetup
    (FrameHeader
       SetupType
       (filterFlags SetupType (flags ++ (view payloadFlags payload)))
       0)
    versionMajor
    versionMinor
    keepaliveTime
    maxLifeTime
    token
    metadataMimeType
    dataMimeType
    payload
    
newFrameLease ttl numberOfRequests Nothing =
  FrameLease (FrameHeader LeaseType [EmptyFlag] 0) ttl numberOfRequests Nothing
newFrameLease ttl numberOfRequests m =
  FrameLease (FrameHeader LeaseType [MetadataFlag] 0) ttl numberOfRequests m

newFrameResume token last client proto =
  FrameResume (FrameHeader ResumeType [EmptyFlag] 0) token last client proto
newFrameResumeOK pos =
  FrameResumeOK (FrameHeader ResumeOKType [EmptyFlag] 0) pos
