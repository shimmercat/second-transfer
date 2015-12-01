#!/bin/bash
USERNAME=alcides
g++-5 -std=c++11 -shared -Wl,-undefined -Wl,dynamic_lookup  -I/Users/$USERNAME/.stack/programs/x86_64-osx/ghc-7.10.2/lib/ghc-7.10.2/include/  -I/usr/local/include/botan-1.11 -L/usr/local/lib -lbotan-1.11 -framework Security cbits/enable_tls.cpp -o libenable_tls.dylib
cp ./libenable_tls.dylib /usr/local/lib
