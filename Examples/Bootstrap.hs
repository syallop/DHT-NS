{-# LANGUAGE ScopedTypeVariables, TypeOperators, FlexibleContexts #-}
module Main where

import DHT.Contact

import DHT.NS
import DHT.NS.Logging

import Control.Concurrent

main :: IO ()
main = do
  logging <- newDHTNSLogging
  let owningAddr = Addr "127.0.0.1" 7770
      ourDHTAddr = Addr "127.0.0.1" 6660
      hashSize   = 8

  dhtns <- do config <- mkDHTNSConfig ourDHTAddr hashSize logging Nothing
              newDHTNSNode config owningAddr

  putStrLn "Waiting..."
  threadDelay 10000000000
  putStrLn "Bootstrap DEAD"

