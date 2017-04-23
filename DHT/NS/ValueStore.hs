{-|
Stability : experimental

Defines a simple in-memory hashmap ValueStore for use in a DHT.
 -}
module DHT.NS.ValueStore
  (newDHTNSValueStore
  )
  where

import DHT
import DHT.ID

import Control.Concurrent
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as Lazy

import Data.List (intercalate)

-- The state of stored values is a MVar reference to a Map
type ValState = MVar (Map.Map ID Lazy.ByteString)

-- Insert a new value
valInsert :: ValState -> ValInsertF IO
valInsert valState vID val = do
  vs <- takeMVar valState
  let vs' = Map.insert vID val vs
  putMVar valState vs'

-- Lookup a value
valLookup :: ValState -> ValLookupF IO
valLookup valState vID = do
  vs <- readMVar valState
  let mVal = Map.lookup vID vs
  return mVal

-- | Create a new empty ValueStore to be used by a DHT.
newDHTNSValueStore :: IO (ValueStoreOp IO)
newDHTNSValueStore = mkValueStore <$> newValState
  where
    mkValueStore valState = ValueStoreOp (valInsert valState) (valLookup valState)

    newValState :: IO ValState
    newValState = newMVar Map.empty

