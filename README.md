
Developer README
================

Introduction
------------

This is a library for creating HTTP/2 servers.

To see some introductory docs, please check the Hackage page or
the file hs-src/SecondTransfer.hs.

Supported platforms
-------------------

At the moment, we  support both Linux and Mac OS X. We haven't tested the library in
Windows, but off the top of my head I can not think on any too Unix specific thing
that we are using. 

Building and installing
-----------------------

The preferred method of installing SecondTransfer is through [Stack](https://github.com/commercialhaskell/stack).
SecondTransfer uses [Botan](http://botan.randombit.net/) for its TLS layer, but
the default build disables the library to play nice with Stack's build servers.
Enable it by installing Botan in your preferred location, switching on the flag
`enable-botan` (you can do that in your stack.yaml file) and adjusting the necessary
include directories (through `extra-include-dirs` and `extra-lib-dirs`, also in
stack.yaml).

We use a extensible TLS mechanism, so if you would rather use a recent version
of OpenSSL, look for the typeclass `TLSContext` and implement it.


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
