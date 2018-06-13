-- | Main interface to our tcpdump-based entropy generator
-- note: uses combinators from Util.hs to do the bulk of the
-- pointer juggling.

{-# LANGUAGE OverloadedStrings #-}
module Tcp ( tcpdump, tcpstream ) where


import           Data.Functor ((<$))
import           System.IO    (Handle)
import           Util         (allocateBlock, buffer, exec, pipeTo)

-- | Generate entropy from en0, reading in 256 bytes to stderr and stdout
tcpdump :: IO ()
tcpdump =exec allocateBlock nullBlock

tcpstream :: IO ()
tcpstream = exec entropyStream nullStream

-- | Handle streaming 256 byte blocks to stderr
nullBlock
  :: Handle -- ^stderr will be the handle used
  -> IO () -- ^dump to stderr
nullBlock = pipeTo (\h p -> () <$ buffer h p)

-- | Stream tcp dump to stdout until ^C is received
entropyStream
  :: Handle -- ^stdout
  -> IO () -- ^dump to stdout
entropyStream h =  allocateBlock h *> entropyStream h

-- | Stream tcpdump stderr until ^C is received
nullStream
  :: Handle -- ^stderr
  -> IO () -- ^dump to stderr
nullStream = pipeTo go
  where go h p = buffer h p *> go h p



