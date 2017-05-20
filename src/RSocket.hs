module RSocket
    ( someFunc
    ) where

import RSocket.Frames
import RSocket.Types
import RSocket.Wireformat.ProtoV10

someFunc :: IO ()
someFunc = putStrLn "someFunc"
