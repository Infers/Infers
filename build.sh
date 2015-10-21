#!/bin/bash

set -e

SOLUTION=Infers.sln

nuget restore $SOLUTION -Verbosity quiet

function build () {
    xbuild /nologo /verbosity:quiet /p:Configuration=$2 $1
}

build $SOLUTION Debug
build $SOLUTION Release
