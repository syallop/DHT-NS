{-# LANGUAGE ScopedTypeVariables, TypeOperators, FlexibleContexts, RankNTypes #-}
module Main where

import DHT.Contact
import DHT.NS.Logging
import DHT.NS

main :: IO ()
main = do
  logging <- newDHTNSLogging
  let owningAddr       = Addr "127.0.0.1" 7772
      ourDHTAddr       = Addr "127.0.0.1" 6662
      bootstrapDHTAddr = Addr "127.0.0.1" 6660
      hashSize         = 8

  dhtns <- do config <- mkDHTNSConfig ourDHTAddr hashSize logging (Just bootstrapDHTAddr)
              newDHTNSNode config owningAddr

  putStrLn "Looking up owner of 'foo','bar' and 'baz'"
  mOwner0 <- lookupNameWithDHTNS dhtns "foo"
  print mOwner0

  mOwner1 <- lookupNameWithDHTNS dhtns "bar"
  print mOwner1

  mOwner2 <- lookupNameWithDHTNS dhtns "baz"
  print mOwner2

  putStrLn "Lookuper DEAD"

