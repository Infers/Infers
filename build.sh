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

function build () {
    $BUILD /nologo /verbosity:quiet /p:Configuration=$2 $1
}

build $SOLUTION Debug
build $SOLUTION Release

.paket/paket pack output . templatefile Infers.paket.template
.paket/paket pack output . templatefile Infers.Rep.paket.template
.paket/paket pack output . templatefile Infers.Toys.paket.template
