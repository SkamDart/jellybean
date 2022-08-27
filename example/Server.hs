{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent
import Data.Binary
import GHC.Generics
import Network.Jellybean
import qualified Network.Socket as N
import System.Environment


data Message
  = Req String
  | Res String
  deriving (Binary, Generic, Show)

main :: IO ()
main = getArgs >>= \case
    [t] -> serverSocket t >>= runServer >>= N.close
    _ -> error "usage <topic>"

runServer :: N.Socket -> IO N.Socket
runServer s = do
  listen s $ \(m :: Message) -> do
    print m
    threadDelay 1000000
    pure (Res "World")
  pure s

