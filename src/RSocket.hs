module RSocket
    ( someFunc
    ) where

import RSocket.Frames
import RSocket.Types
import RSocket.ProtoV10

someFunc :: IO ()
someFunc = putStrLn "someFunc"
