#!/bin/bash -e

SOLUTION=Infers.sln

if hash xbuild &> /dev/null ; then
    BUILD=xbuild
elif hash msbuild.exe &> /dev/null ; then
    BUILD=msbuild.exe
else
    echo "Couldn't find build command."
    exit 1
fi

if hash paket &> /dev/null ; then
    PAKET=paket
else
    PAKET=.paket/paket.exe
fi

function build () {
    $BUILD /nologo /verbosity:quiet /p:Configuration=$2 $1
}

if [ "$1" != "" ] ; then
    build $SOLUTION "$*"
else
    build $SOLUTION Debug
    build $SOLUTION Release

    $PAKET pack output . templatefile Infers.paket.template
    $PAKET pack output . templatefile Infers.Rep.paket.template
    $PAKET pack output . templatefile Infers.Toys.paket.template
fi

