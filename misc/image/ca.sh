#!/bin/sh
cd ../../src
for a in amd64 i686 arm64 arm32hf alpha ppc32 ppc64el risc64 s390x loong sparc sh4 ; do
  java -Xmx256m -jar rtr.jar test image ../misc/image/platform.$a ../misc/image/image.jvm
  done
java -Xmx256m -jar rtr.jar test image ../misc/image/platform.amd64 ../misc/image/image.fbsd
java -Xmx256m -jar rtr.jar test image ../misc/image/platform.amd64 ../misc/image/image.nbsd
