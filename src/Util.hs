{-# LANGUAGE ScopedTypeVariables #-}
module Util
  ( buffer
  , allocateBlock
  , pipeTo
  , exec
  ) where

import           Control.Concurrent (forkIO)
import           Control.Exception  (IOException, catch)
import           Data.Text.Lazy     (Text, unpack)
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

-- | Create shell process, executing a command and handling
-- writing to stdout via some write function, and
-- handling stderr output with another.
exec
  :: (Handle -> IO ()) -- ^ handle stdout
  -> (Handle -> IO ()) -- ^ handle stderr
  -> Text -- ^ command to run
  -> IO ()
exec _out _err cmd = do
  (_, Just hout, Just herr, _) <-
    createProcess (shell (unpack cmd))
    { std_out = CreatePipe
    , std_err = CreatePipe
    }
  forkIO (_err herr)
  _out hout



