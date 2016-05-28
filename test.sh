#!/bin/bash -e

./build.sh

if hash mono &> /dev/null ; then
  RUN=mono
else
  RUN=
fi
