{-# LANGUAGE
    DataKinds
  , GADTs
  , LambdaCase
  , MultiParamTypeClasses
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TypeOperators
  , FlexibleContexts
  #-}
{-|
Stability : experimental

Defines a thin Name Server API backed by a DHT providing operations for registering and looking up names
and their associated IP address and ports.

Usage:
- Create configuration with 'mkDHTNSConfig'.
- Pass configuration to 'newDHTNSNode' to begin running, keeping
  data alive and responding to other DHTs in the background.
- Pass the returned 'DHTNS' handle into lookup and registering
- functions as required.
-}
module DHT.NS
  ( DHTConfig (..)
  , mkDHTNSConfig

  , DHTNS ()
  , newDHTNSNode

  , lookupNameWithDHTNS
  , registerNameWithDHTNS
  )
  where

import Control.Concurrent
import Data.Time.Clock.POSIX
import System.Random

import DHT
import DHT.Contact
import DHT.ID
import DHT.Types

import DHT.NS.Messaging
import DHT.NS.RoutingTable
import DHT.NS.ValueStore

import Data.ByteString.Lazy (ByteString)

import Control.Monad (void)

import Data.String.Conv

-- TODO: Code reuse
type Name = String

data DHTNS = DHTNS
  {_dhtnsOwningAddr :: Addr             -- ^ The Address of the owner of the name NOT the address of any DHT component
  ,_dhtnsConfig     :: DHTConfig DHT IO
  }

-- Create a config object for a DHTNS
mkDHTNSConfig
  :: Addr                  -- ^ Address to bind the DHT on
  -> Int                   -- ^ Hash size for keys
  -> LoggingOp IO          -- ^ Possible logging
  -> Maybe Addr            -- ^ Possible bootstrap
  -> IO (DHTConfig DHT IO)
mkDHTNSConfig ourDHTAddr hashSize logging mBootstrapAddr = do
  now          <- timeF
  routingTable <- newDHTNSRoutingTable maxBucketSize ourDHTID now hashSize
  valueStore   <- newDHTNSValueStore
  messaging    <- newDHTNSMessaging (maxPortLength,ourDHTPort)

  let ops = DHTOp {_dhtOpTimeOp         = timeF
                  ,_dhtOpRandomIntOp    = randF
                  ,_dhtOpMessagingOp    = messaging
                  ,_dhtOpRoutingTableOp = routingTable
                  ,_dhtOpValueStoreOp   = valueStore
                  ,_dhtOpLoggingOp      = logging
                  }
  return $ DHTConfig ops ourDHTAddr hashSize mBootstrapAddr
  where
    timeF :: IO Time
    timeF = round <$> getPOSIXTime

    randF :: IO Int
    randF = randomRIO (0,maxBound)

    Addr _ourDHTIP ourDHTPort = ourDHTAddr
    ourDHTID = mkID ourDHTAddr hashSize

    maxPortLength = 5

    maxBucketSize = 8

newDHTNSNode
  :: DHTConfig DHT IO -- ^ DHT configuration
  -> Addr             -- ^ The address of the owner of all registered names
  -> IO DHTNS         -- ^ A handle to the DHTNS to be threaded into operations.
newDHTNSNode dhtConfig owningAddr = do
  -- Initiate the messaging subsystem so DHT requests and responses are sent and
  -- received in the background
  void $ forkIO $ void $ startMessaging dhtConfig

  -- Bootstrap off any given bootstrap node to get some contacts.
  void $ runDHT dhtConfig bootstrap

  return $ DHTNS owningAddr dhtConfig

-- A request for the DHTNS to lookup a name that may be
-- registered, and if so, return the 'Addr' of the owner.
lookupNameWithDHTNS :: DHTNS -> Name -> IO (Maybe Addr)
lookupNameWithDHTNS dhtns name = do
  let dhtConfig = _dhtnsConfig dhtns
      hashSize  = _dhtConfigHashSize dhtConfig
  res <- runDHT dhtConfig $ findValue $ mkID (toS name :: ByteString) hashSize
  case res of
    -- DHT Error.
    -- TODO: Indicate error?
    Left _err
      -> return Nothing

    -- Got some response
    Right (_neighbours,mBs)
      -> case mBs of
           -- No such value exists in DHT
           Nothing
             -> return Nothing

           -- Value exists. Unsafely try and read it as an address.
           -- TODO: Don't be so unsafe.
           Just bs
             -> let r = read . toS $ bs
                   in return $ Just r

-- A request for our DHT to register us as being the owner of a given name.
-- Bool indicates success.
registerNameWithDHTNS :: DHTNS -> Name -> IO Bool
registerNameWithDHTNS dhtns name = do
  let dhtConfig     = _dhtnsConfig     dhtns
      ourOwningAddr = _dhtnsOwningAddr dhtns
  res <- runDHT dhtConfig $ store (toS name) (toS . show $ ourOwningAddr)
  case res of
    -- DHT Error.
    -- TODO: Indicate error?
    Left _err
      -> return False

    -- Registered against nameID
    Right _nameID
      -> return True

