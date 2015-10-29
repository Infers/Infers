#!/bin/bash -e

./build.sh

for fsi in fsharpi fsianycpu.exe fsi.exe ; do
    if hash $fsi &> /dev/null ; then
        rlwrap -t dumb $fsi --use:Infers.fsx
        exit
    fi
done
