#!/bin/sh
cd ../../src
java -Xmx1024m -jar rtr.jar test image ../misc/image/platform.i686 ../misc/image/image.libr
for a in amd64 i686 arm64 arm32hf alpha ppc32 ppc64el risc64 s390x loong sparc sh4 ; do
  java -Xmx1024m -jar rtr.jar test image ../misc/image/platform.$a ../misc/image/image.jvm
  done
java -Xmx1024m -jar rtr.jar test image ../misc/image/platform.amd64 ../misc/image/image.fbsd
java -Xmx1024m -jar rtr.jar test image ../misc/image/platform.amd64 ../misc/image/image.nbsd
