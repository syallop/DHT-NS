{-|
Stability : experimental

Wraps DHT.Routing into a DHT RoutingTable using MVar shared state.
 -}
module DHT.NS.RoutingTable
  (newDHTNSRoutingTable
  )
  where

import Prelude hiding (lookup)

import DHT
import DHT.Contact
import DHT.ID
import DHT.Routing
import DHT.Types

import Control.Concurrent

type RTState = MVar Routing

-- Insert a new address into the routingtable, pinging questionable nodes with
-- the ping function to update them if required.
rtInsert :: RTState -> Addr -> Time -> (Addr -> DHT IO Bool) -> DHT IO ()
rtInsert rtState addr time pingValue = do
  rt  <- liftDHT $ takeMVar rtState
  hashSize <- askHashSize
  rt' <- insert addr time pingValue hashSize rt
  liftDHT $ putMVar rtState rt'

-- Lookup the Contact associated with an 'ID', also return k neighbour contacts
-- relative from the enquiring Addr.
rtLookup :: RTState -> Int -> Addr -> ID -> Time -> IO ([Contact],Maybe Contact)
rtLookup rtState hashSize enquirerAddr targetID now = do
  rt <- takeMVar rtState
  let (rt',res) = lookup enquirerAddr targetID now hashSize rt
  putMVar rtState rt'
  return res

newDHTNSRoutingTable :: Int -> ID -> Time -> Int -> IO (RoutingTableOp DHT IO)
newDHTNSRoutingTable size ourID now hashSize = mkRoutingTable <$> newRTState
  where
    mkRoutingTable rtState = RoutingTableOp (rtInsert rtState)
                                            (rtLookup rtState hashSize)
                                            (pure size)
    newRTState = newMVar $ empty size ourID now

