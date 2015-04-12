	
Developer README
================

Introduction
------------

This is an early-stage and very experimental library to create HTTP/2 servers
using Haskell. 

To see the package docs, please check the Hackage page or 
the file hs-src/SecondTransfer.hs.

Building and installing
-----------------------

You need Haskell GHC compiler installed (version 7.8.3 at least). You also 
need OpenSSL 1.0.2, since the ALPN feature and some very recent cypher-suites
are needed by HTTP/2. this source distribution will try to find them at  
directory `/opt/openssl-1.0.2`, but you should be able to 
alter the options using `cabal configure`. This package uses Haskell's FFI to interface with OpenSSL.

Provided that you have all the dependencies, you should be able to just do:

    $ cabal install second-transfer

Example
-------

There is a very basic example at `tests/tests-hs-src/compiling_ok.hs`. 

Roadmap
-------

Done:

- Version 0.1: Having something that can run. No unit-testing, nothing 
               fancy. 

- Version 0.2: Absolutely minimal amount of unit tests.

Pending:

- Version 0.3: Better examples of usage. 

- Version 0.4: By-the-book stream state management. In particular, ensure 
               that we are not allowing frames to come off-order from the 
               other peer. 

- Version 0.5: Guaranties about early resource release...
