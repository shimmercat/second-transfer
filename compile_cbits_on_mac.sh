#!/bin/bash

platform='unkown'
unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
   platform='linux'
elif [[ "$unamestr" == 'FreeBSD' ]]; then
   platform='freebsd'
elif [[ "$unamestr" == 'Darwin' ]]; then 
   platform='darwin'
elif [[ "$unamestr" == *"MINGW64"* ]]; then 
   platform='windows'
fi

GHC_VERSION="7.10.3"

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

if [[ $platform == 'darwin' ]] ; then 
    libname=libsecond_transfer__enable_tls.dylib
    iplace=/usr/local/shimmercat-build/
    CXX=g++-5.0
    botan_name=botan-1.11 
    botan_location=/usr/local
    botan_includes=$botan_location/include/botan-11
    botan_lib_dir=/usr/local/shimmercat-build 
    pushd $THIS_DIR/..
    placement_path=`stack path --local-install-root`/lib/x86_64-osx-ghc-$GHC_VERSION/
    extra_compile_flags="-Wl,-install_name -Wl,@rpath/$libname \
    -Wl,-rpath -Wl,$iplace"
    popd
    output_place=$THIS_DIR
elif [[ $platform == 'windows' ]] ; then
    libname=second_transfer__enable_tls.dll
    iplace=$THIS_DIR/cbits
    CXX="stack exec -- g++"
    # CXX="/c/cygwin64/opt/windows_64/bin/g++"
    botan_name=botan
    build_dir=/c/shimmercat-build
    botan_location=$build_dir
    botan_includes=$botan_location/include/
    botan_lib_dir=$botan_location/lib
    extra_compile_flags="-DBUILDING_DLL -L$THIS_DIR/cbits"
    extra_compile_flags2="-lsecond_transfer"
    pushd $THIS_DIR/..
    placement_path=`stack path --local-install-root`/lib/x86_64-windows-ghc-$GHC_VERSION/
    popd
    output_place=/c/shimmercat-build/lib
fi 


echo "#placement_path: $placement_path"

target_dependent=$placement_path/second-transfer-*-*/libHSsecond-transfer-*-*-ghc*.dylib


if [[ $platform == 'windows' ]] ; then
    stack exec -- dlltool -d $THIS_DIR/second_transfer.def -l $build_dir/second_transfer.lib
fi

$CXX \
    -std=c++11 -shared \
    -Wl,-undefined -Wl,dynamic_lookup \
    -I$ghc_includes  -I$botan_includes \
    -l$botan_name \
    -L$botan_lib_dir \
    $extra_compile_flags \
    \
    $THIS_DIR/cbits/enable_tls.cpp \
    $extra_compile_flags2 \
    \
    -o $output_place/$libname

echo "Compile command result: $?"


if [[ $platform == 'darwin' ]] ; then 
	cp $THIS_DIR/$libname $placement_path
	cp $THIS_DIR/$libname $iplace
	echo "#iplace: " $iplace
	echo "#target_dependent: " $target_dependent
	if [[ $platform == 'darwin' ]] ; then 
	    for one_target_dependent in $target_dependent ; do 
	       install_name_tool -add_rpath $iplace $one_target_dependent || echo "Iplace already set"
	    done
	fi 
	rm $THIS_DIR/$libname
	echo "DON't forget to run tools/fix_mac_dylibs.sh!!!"
fi

