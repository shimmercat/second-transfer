
Developer README
================

Introduction
------------

This is a library for creating HTTP/2 servers.

To see some introductory docs, please check the Hackage page or
the file hs-src/SecondTransfer.hs.

Supported platforms
-------------------

At the moment, we only support Linux. But there are plans to support other platforms.

Building and installing
-----------------------

The preferred method of installing SecondTransfer is through [Stack](https://github.com/commercialhaskell/stack).
SecondTransfer embeds [Botan](http://botan.randombit.net/) for its TLS layer. 
Therefore, a modern C++ compiler (e.g., g++ 4.8) should be available at compile time. 

We used OpenSSL in the past
and there is a possibility of supporting it again in the future. 

Running the tests
-----------------

There are two sets of tests: normal Haskell tests and a custom test suite called Suite 1 that requires 
Stack, Python 3.4+, Redis running in localhost/standard port with DB 3 erasable, and Numpy. 
To run Suite 1, SecondTransfer should be compiled with the "Monitoring" flag enabled. 

Example
-------

There is a very basic example at `tests/tests-hs-src/compiling_ok.hs`, and a somewhat more complicated one at
`examples/attempt_bust`; that one shows how to do HTTP/2.0 push from the library.


Development
-----------

Uploading documentation (provided you have access to the package in Hackage):

    $ ./hackage-upload-docs.sh second-transfer 0.5.4.0 <hackage-user> <hackage-password>
