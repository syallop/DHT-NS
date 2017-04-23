{-|
Stability : experimental

Defines a simple logging system which outputs all logged strings to stdout immediately.
 -}
module DHT.NS.Logging
  ( newDHTNSLogging
  ) where

import Control.Concurrent
import Control.Monad

import DHT

-- | Create a new logging system which outputs to stdout
newDHTNSLogging :: IO (LoggingOp IO)
newDHTNSLogging = do
  loggingState <- newChan
  void $ forkIO $ forever $ readChan loggingState >>= putStrLn
  return $ Just (writeChan loggingState)

