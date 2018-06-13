module Main where

import           System.Environment (getArgs)
import           System.IO          (BufferMode (NoBuffering),
                                     hSetBuffering, stderr, stdout)
import           Tcp                (tcpdump, tcpstream)


-- | We support a very minimal number of args here to make it easier
parseArgs :: [String] -> IO ()
parseArgs ["--tcp"]    = tcpdump
parseArgs ["--stream"] = tcpstream
parseArgs ["--tcp -s"] = tcpstream
parseArgs _            = tcpdump


-- | Note: Tcp dump usually requires sudo access. If Main is not
-- outputing anything, that's probably the problem :)
--
-- Source: 15 minutes of tearing my hair out
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  getArgs >>= parseArgs

