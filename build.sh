#!/bin/bash -e

if hash xbuild &> /dev/null ; then
  BUILD=xbuild
  RUN=mono
elif hash msbuild.exe &> /dev/null ; then
  BUILD=msbuild.exe
  RUN=
else
  echo "Couldn't find build command."
  exit 1
fi

PAKET=.paket/paket.exe

if [ ! -f $PAKET ] ; then
  $RUN .paket/paket.bootstrapper.exe
fi

$RUN $PAKET install

function build () {
  $BUILD /nologo /verbosity:quiet /p:Configuration=$2 $1
}

for SOLUTION in *.sln ; do
  if [ "$1" != "" ] ; then
      build $SOLUTION "$*"
  else
    for config in Debug Release ; do
      build $SOLUTION $config
    done

    for template in *.paket.template ; do
      $RUN $PAKET pack output . templatefile $template
    done
  fi
done
