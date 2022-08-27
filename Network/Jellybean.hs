{- |
Module      :  Network.Jellybean
Description :  A small, opinionated ipc.
Copyright   :  (c) Cameron Dart
License     :  NONE

Maintainer  :  cdart2@illinois.edu
Stability   :  experimental
Portability :  non-portable (UNIX networking apis)

Jellybean is a simple rpc per connection framework. The intended usage is for
very simple message passing between two processes on the same device.
-}
module Network.Jellybean where

import Data.Binary

import qualified Network.Socket as N
import qualified Network.Socket.ByteString.Lazy as NBL
import Control.Monad

-- | Maximum size of an individual message.
maxMessageSize :: Integral a => a
maxMessageSize = 8192

-- | Connect to a jellybean server with the provided name.
socket
  :: FilePath -- ^ Topic
  -> IO N.Socket
socket fp = do
  s <- N.socket N.AF_UNIX N.Stream 0
  N.connect s (N.SockAddrUnix fp)
  pure s

-- | Send a message to a connected socket.
send
  :: Binary a
  => N.Socket -- ^ Connected socket
  -> a -- ^ Data to send
  -> IO ()
send s m = NBL.sendAll s (encode m)

-- | Receive a message from a connected socket.
recv
  :: Binary a
  => N.Socket -- ^ Connected socket
  -> IO a
recv s = do
  b <- NBL.recv s maxMessageSize
  pure (decode b)

-- | Call the jellybean server.
call
  :: (Binary a, Binary b)
  => N.Socket -- ^ Connected client socket
  -> a -- ^ Data to send
  -> IO b
call s r = do
  send s r
  recv s

listen
  :: (Binary a, Binary b)
  => N.Socket -- ^ Connected server socket.
  -> (a -> IO b) -- ^ Action to be applied to deserialized data
  -> IO ()
listen s f = do
  (cs, _) <- N.accept s
  forever $ do
    m <- recv cs
    x <- f m
    send cs x

-- | Create and bind a socket to be used by a jellybean server.
serverSocket
  :: FilePath -- ^ Topic
  -> IO N.Socket
serverSocket fp = do
  s <- N.socket N.AF_UNIX N.Stream 0
  N.bind s (N.SockAddrUnix fp)
  N.listen s 1
  pure s

