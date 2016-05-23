#!/bin/bash -e

./build.sh Debug

if hash fsharpi &> /dev/null ; then
  rlwrap -t dumb fsharpi --use:Infers.fsx
else
  rlwrap -t dumb fsianycpu.exe --use:Infers.fsx
fi
