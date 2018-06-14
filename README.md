## Symbiont.io Question 2018

***"create a clone of `cat /dev/random`, generating your own entropy"***


This project uses `tcpdump` and `hexdump` to generate hardware entropy in the vein of `/dev/random`. While the `tcpdump` data is generated from hardware, it is not impervious to attack due to nature of packet reporting from the `en0` interface, which we use, which may be exploited by sending known packet data to the interface. To make this more cryptograpically secure, a CSPRNG such as Salsa or ChaCha should scramble the packet data prior to outputting the stream.

To run the project, remember that on Mac OS X, at least, to use `tcpdump`, superuser access is necessary, so one must run `sudo cabal run` and provide any of the array of the following options:

- [--tcpdump | --tcp | noargs]: Dumps a single 256 byte block of tcp data
- [--tcp --s | --stream]: Streams 256 byte blocks of tcp data

If the interviewer requests it, I would love to implement a Cha-Cha or Salsa20 stream cipher and add those options as well.
