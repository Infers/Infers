#!/bin/bash -e

if hash xbuild &> /dev/null ; then
  BUILD=xbuild
  RUN=mono
elif hash msbuild.exe &> /dev/null ; then
  BUILD="msbuild.exe /maxcpucount"
  RUN=
else
  echo "Couldn't find build command."
  exit 1
fi

PAKET=.paket/paket.exe

$RUN .paket/paket.bootstrapper.exe -s --max-file-age=60
if [ ! -x $PAKET ] ; then
  chmod +x $PAKET
fi

$RUN $PAKET --silent restore

function build () {
  $BUILD /nologo /verbosity:quiet /p:Configuration="$2" "$1"
}

for SOLUTION in *.sln ; do
  if [ "$1" != "" ] ; then
      build "$SOLUTION" "$*"
  else
    for CONFIG in Debug Release ; do
      build "$SOLUTION" $CONFIG
    done

    for TEMPLATE in *.paket.template ; do
      $RUN $PAKET pack output . templatefile "$TEMPLATE"
    done
  fi
done
