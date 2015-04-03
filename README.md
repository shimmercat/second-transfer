	
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
are needed by HTTP/2. In this source distribution, I have set them to live in the 
directory `/opt/openssl-1.0.2`, but you should be able to 
alter the options using `cabal configure`. This package uses Haskell's foreign function 
interface to interface with OpenSSL.

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

Pending:

- Version 0.2: Have some unit tests. A minimal amount of them