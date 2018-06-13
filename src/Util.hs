{-# LANGUAGE ScopedTypeVariables #-}
module Util
  ( buffer
  , allocateBlock
  , pipeTo
  , exec
  ) where

import           Control.Concurrent (forkIO)
import           Control.Exception  (IOException, catch)
import           Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import           Foreign.Ptr        (Ptr)
import           System.Exit        (exitSuccess)
import           System.IO          (Handle, hGetBuf, hPutBuf, stderr,
                                     stdout)
import           System.Process     (StdStream (CreatePipe), createProcess,
                                     proc, shell, std_err, std_out)

-- | Allocate standard 256 byte block to make the eta expansions a little nicer
buffer
  :: Handle -- ^handle to buffer
  -> Ptr a -- ^pointer connecting handle
  -> IO Int
buffer h p = hGetBuf h p 256

-- | Allocate 256 bytes to buffer, piping to stderr in the case of IOException
allocateBlock
  :: Handle -- ^allocate 256 foreign bytes to buffer on this handle
  -> IO ()
allocateBlock h = do
  ptr <- mallocForeignPtrBytes 256
  let buf p = do
        i <- buffer h p
        catch (hPutBuf stdout p i) (\(_ :: IOException) -> exitSuccess)
  withForeignPtr ptr buf

-- | Standard combinator for shapes that look like `hPutBuf`. Makes things a little cleaner
pipeTo
  :: (Handle -> Ptr a -> IO ())
  -> Handle -- ^ handle to pipe `f` into
  -> IO ()
pipeTo f h = do
  p <- mallocForeignPtrBytes 256
  withForeignPtr p (f h)

-- | call tcpdump and allow provide handlers to work with stderr and stdout outcomes
-- The tcpdump arguments are as follows:
--
-- K: don't verify checksums
-- n: don't convert addresses to names
-- O: no optimizations
-- S: print absolute TCP sequences numbers
-- x: to hex + headers
-- vvv: highest verbosity
-- i: interface
exec
  :: (Handle -> IO ()) -- ^ handle stdout
  -> (Handle -> IO ()) -- ^ handle stderr
  -> IO ()
exec _out _err = do
  (_, Just hout, Just herr, _) <-
    createProcess (shell "tcpdump -KnOSx -vvv -i en0 | hexdump -x")
    { std_out = CreatePipe, std_err = CreatePipe }
  forkIO (_err herr)
  _out hout



