{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module RSocket.Frames where

import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.Map.Strict      as Map
import           Data.Tuple
import           Data.Word
import           Data.String
import           Data.Int
import           Control.Lens         hiding (Empty)

type StreamId = Word32 
type FrameFlags = Word16
type ResumePosition = Int64
newtype Metadata =
  Metadata B.ByteString
  deriving (Eq, Show)
newtype Payload =
  Payload B.ByteString
  deriving (Eq, Show)
type LeaseTTL = Word32
type LeaseNumOfRequests = Word32 
type RequestN = Int32 
  

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
  | InvalidRequestError
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
    , (InvalidRequestError, 0x0204)
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

setFrameFlag :: Bits a => [FrameFlag] -> a-> a
setFrameFlag [] v     = v
setFrameFlag (f:fs) v = setFrameFlag fs (setBit v (frameFlagMap Map.! f))

isFrameFlag :: Bits a => FrameFlag -> a -> Bool
isFrameFlag f v = testBit v (frameFlagMap Map.! f)

allFrameFlags :: Bits a => a -> [FrameFlag]
allFrameFlags a = Map.foldlWithKey' testFlag [] frameFlagMap
  where
    testFlag xs k v
      | isFrameFlag k a = k : xs
      | otherwise = xs




data ResumeIdentificationToken = ResumeIdentificationToken [Word8]
                                deriving (Eq, Show, Ord)


-- FIXME errorData SHOULD be UTF8 encoded string without \NUL
declareLenses
  [d|

  data ProtocolVersion = ProtocolVersion{protocolVersionMajor ::
                                         Word16,
                                         protocolVersionMinor :: Word16}
                       deriving (Eq, Show, Ord)

  data FrameHeader = FrameHeader{headerStreamId :: !StreamId,
                                 headerType :: !FrameType, headerFlags :: ![FrameFlag]}
                   deriving (Eq, Show)

  data SetupParameters = SetupParameters{version :: !ProtocolVersion,
                                         keepaliveTime :: !Word32, maxLifetime :: !Word32,
                                         setupResumeToken :: !ResumeIdentificationToken,
                                         metadataMimeType :: !String, dataMimeType :: !String}
                       deriving (Eq, Show)

  data Frame = FrameSetup{header :: !FrameHeader,
                          setupParameters :: !SetupParameters, metadata :: Maybe Metadata,
                          payload :: Maybe Payload}
             | FrameLease{header :: !FrameHeader, leaseTTL :: !LeaseTTL,
                          leaseNumOfRequests :: !LeaseNumOfRequests,
                          metadata :: Maybe Metadata}
             | FrameKeepAlive{header :: !FrameHeader,
                              lastPosition :: !ResumePosition, payload :: Maybe Payload}
             | FrameRequestResponse{header :: !FrameHeader,
                                    metadata :: Maybe Metadata, payload :: Maybe Payload}
             | FrameRequestFNF{header :: !FrameHeader,
                               metadata :: Maybe Metadata, payload :: Maybe Payload}
             | FrameRequestStream{header :: !FrameHeader, requestN :: !RequestN,
                                  metadata :: Maybe Metadata, payload :: Maybe Payload}
             | FrameRequestChannel{header :: !FrameHeader,
                                   requestN :: !RequestN, metadata :: Maybe Metadata,
                                   payload :: Maybe Payload}
             | FrameRequestN{header :: !FrameHeader, requestN :: !RequestN}
             | FrameCancel{header :: !FrameHeader}
             | FramePayload{header :: !FrameHeader, metadata :: Maybe Metadata,
                            payload :: Maybe Payload}
             | FrameError{header :: !FrameHeader, errorCode :: !ErrorCode,
                          errorData :: Maybe Payload}
             | FrameMetadataPush{header :: !FrameHeader,
                                 metadata :: Maybe Metadata}
             | FrameResume{header :: !FrameHeader,
                           protocolVersion :: !ProtocolVersion,
                           resumeToken :: !ResumeIdentificationToken,
                           lastServerPosition :: !ResumePosition,
                           firstClientPosition :: !ResumePosition}
             | FrameResumeOK{header :: !FrameHeader,
                             lastClientPosition :: !ResumePosition}
             deriving (Eq, Show)
  |]

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

validateFlags :: FrameType -> [FrameFlag] -> Maybe Metadata -> [FrameFlag]
validateFlags t f Nothing = filterFlags t f
validateFlags t f (Just _) = filterFlags t (f ++ [MetadataFlag])

-- FIXME maxKeepaliveTime maxLifetime and >0 bound checks
frameSetup flags parameters metadata payload =
  FrameSetup
    (FrameHeader 0 SetupType (validateFlags SetupType flags metadata))
    parameters
    metadata
    payload

frameLease ttl numberOfRequests m =
  FrameLease
    (FrameHeader 0 LeaseType (validateFlags LeaseType [] m))
    ttl
    numberOfRequests
    m

frameKeepAlive flags position payload =
  FrameKeepAlive
    (FrameHeader 0 KeepAliveType (validateFlags KeepAliveType flags Nothing))
    position
    payload

frameRequestResponse streamId flags metadata payload =
  FrameRequestResponse
    (FrameHeader
       streamId
       RequestResponseType
       (validateFlags RequestResponseType flags metadata))
    metadata
    payload

frameRequestFNF streamId flags metadata payload =
  FrameRequestFNF
    (FrameHeader
       streamId
       RequestFNFType
       (validateFlags RequestFNFType flags metadata))
    metadata
    payload

frameRequestStream streamId flags n metadata payload =
  FrameRequestStream
    (FrameHeader
       streamId
       RequestStreamType
       (validateFlags RequestStreamType flags metadata))
    n
    metadata
    payload

frameRequestChannel streamId flags n metadata payload =
  FrameRequestChannel
    (FrameHeader
       streamId
       RequestChannelType
       (validateFlags RequestChannelType flags metadata))
    n
    metadata
    payload

frameRequestN streamId n =
  FrameRequestN (FrameHeader streamId RequestNType [EmptyFlag]) n
frameCancel streamId = FrameCancel (FrameHeader streamId CancelType [])

framePayload streamId flags metadata payload =
  FramePayload
    (FrameHeader
       streamId
       PayloadType
       (validateFlags PayloadType flags metadata))
    metadata
    payload

frameError streamId errorCode payload =
  FrameError
    (FrameHeader streamId ErrorType [])
    errorCode
    payload

frameMetadataPush m =
  FrameMetadataPush
    (FrameHeader 0 MetadataPushType [MetadataFlag])
    m
frameResume version token server client =
  FrameResume (FrameHeader 0 ResumeType [EmptyFlag]) version token server client 
frameResumeOK pos = FrameResumeOK (FrameHeader 0 ResumeOKType [EmptyFlag]) pos
