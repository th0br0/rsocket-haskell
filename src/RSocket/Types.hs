{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module RSocket.Types where
import Data.ByteString.Lazy
import Data.Map.Strict as Map
import Data.Word

import Control.Lens

declareLenses [d|

               |]
