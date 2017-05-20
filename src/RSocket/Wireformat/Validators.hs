module RSocket.Wireformat.Validators where

import Data.Int
import RSocket.Frames

validateStreamId :: Int32 -> Maybe StreamId
validateStreamId id
  | id >= 0 = Just $ fromIntegral id
  | otherwise = Nothing

validateFrameType :: Maybe FrameType -> FrameType
validateFrameType Nothing = error "Invalid FrameType"
validateFrameType (Just x) = x
