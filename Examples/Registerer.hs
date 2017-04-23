{-# LANGUAGE ScopedTypeVariables, TypeOperators, FlexibleContexts #-}
module Main where

import DHT.Contact
import DHT.NS
import DHT.NS.Logging

import Control.Concurrent

main :: IO ()
main = do
  logging <- newDHTNSLogging
  let owningAddr       = Addr "127.0.0.1" 7771
      ourDHTAddr       = Addr "127.0.0.1" 6661
      bootstrapDHTAddr = Addr "127.0.0.1" 6660
      hashSize         = 8

  dhtns <- do config <- mkDHTNSConfig ourDHTAddr hashSize logging (Just bootstrapDHTAddr)
              newDHTNSNode config owningAddr

  putStrLn "Claiming names 'foo','bar' and 'baz' ..."
  registerNameWithDHTNS dhtns "foo"
  registerNameWithDHTNS dhtns "bar"
  registerNameWithDHTNS dhtns "baz"
  putStrLn "Registered names, waiting..."
  threadDelay 10000000000
  putStrLn "Registerer DEAD"

