{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad
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
    [t] -> socket t >>= runClient >>= N.close
    _ -> error "usage <topic>"

runClient :: N.Socket -> IO N.Socket
runClient s = do
  void $ forever $ do
    r <- (call s (Req "Hello") :: IO Message)
    print r
  pure s

