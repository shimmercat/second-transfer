#!/bin/bash

platform='unkown'
unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
   platform='linux'
elif [[ "$unamestr" == 'FreeBSD' ]]; then
   platform='freebsd'
elif [[ "$unamestr" == 'Darwin' ]]; then 
   platform='darwin'
fi

if [[ $platform == 'darwin' ]] ; then 
    _predirname=`readlink ${BASH_SOURCE[0]}`
    if [[ $? > 0 ]] ; then 
        _predirname=${BASH_SOURCE[0]}
    fi 
else 
    _predirname=`readlink -f ${BASH_SOURCE[0]}`
fi
THIS_DIR=$(dirname $_predirname)

ghc_includes=$(stack path --global-pkg-db)/../include/
libname=libsecond_transfer__enable_tls.dylib
iplace=/usr/local/shimmercat-build/

pushd $THIS_DIR/..
placement_path=`stack path --local-install-root`/lib/x86_64-osx-ghc-7.10.2/
popd
echo PLACEMENT: $placement_path

target_dependent=$placement_path/second-transfer-*-*/libHSsecond-transfer-*-*-ghc*.dylib


g++-5 \
    -std=c++11 -shared \
    -Wl,-undefined -Wl,dynamic_lookup \
    -I$ghc_includes  -I/usr/local/include/botan-1.11 \
    -lbotan-1.11 \
    -L/usr/local/shimmercat-build \
    -Wl,-install_name -Wl,@rpath/$libname \
    -Wl,-rpath -Wl,$iplace \
    \
    $THIS_DIR/cbits/enable_tls.cpp \
    \
    -o $THIS_DIR/$libname

cp $THIS_DIR/$libname $placement_path
cp $THIS_DIR/$libname $iplace

install_name_tool -add_rpath $iplace $target_dependent || echo "Iplace already set"
rm $THIS_DIR/$libname
