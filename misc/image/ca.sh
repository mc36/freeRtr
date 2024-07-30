#!/bin/sh
cd ../../src
for a in amd64 i686 arm64 arm32 mips64 ppc64 risc64 s390x loong sparc ; do
  java -Xmx256m -jar rtr.jar test image ../misc/image/plat.$a ../misc/image/image.jvm
  done
