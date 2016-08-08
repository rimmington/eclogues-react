{-# LANGUAGE JavaScriptFFI #-}

module Awful (hostname, port) where

import Data.JSString (JSString, unpack)
import System.IO.Unsafe (unsafePerformIO)

foreign import javascript unsafe "window['eclogues_hostname']"
    getHostname :: IO JSString

foreign import javascript unsafe "window['eclogues_port']"
    getPort :: IO Int

{-# NOINLINE hostname #-}
hostname :: String
hostname = unsafePerformIO $ unpack <$> getHostname

{-# NOINLINE port #-}
port :: Int
port = unsafePerformIO getPort
