	
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

Running the tests
-----------------

    $ cabal test


Debugging complicated scenarios
-------------------------------

To access full debugging capabilities, for example from the test suite, use the 
following command from the project's directory:

    $ cabal exec -- ghci -ihs-src -itests/tests-hs-src -itests/support -XCPP -Imacros/ dist/build/cbits/tlsinc.o

Example
-------

There is a very basic example at `tests/tests-hs-src/compiling_ok.hs`. 

Roadmap
-------

Done:

- Version 0.1: Having something that can run. No unit-testing, nothing 
               fancy. 

- Version 0.2: Absolutely minimal amount of unit tests.

- Version 0.3: More sensible logging.

Pending:

- Better examples.

- Epoll I/O management

- Benchmarking.

Internal
--------

Uploading documentation (provided you have access to the package):

    $ ./hackage-upload-docs.sh second-transfer 0.5.4.0 <hackage-user> <hackage-password>
