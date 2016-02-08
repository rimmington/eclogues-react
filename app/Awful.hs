{-# LANGUAGE JavaScriptFFI #-}

module Awful (hostname, port) where

import Data.JSString (JSString, unpack)
import System.IO.Unsafe (unsafePerformIO)

foreign import javascript unsafe "window['eclogues_hostname']"
    getHostname :: IO JSString

foreign import javascript unsafe "window['eclogues_port']"
    getPort :: IO Int

hostname :: String
hostname = unpack $ unsafePerformIO getHostname

port :: Int
port = unsafePerformIO getPort
