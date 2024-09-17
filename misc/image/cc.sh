#!/bin/sh
cd ../../src
for a in i686 amd64 arm32hf mips64 alpha ppc32 ppc64el ppc64eb risc64 s390x sparc loong sh4 m68k hppa arm64 ; do
  java -Xmx256m -jar rtr.jar test image ../misc/image/plat.$a ../misc/image/image.cmp
  bash ../binDsk/r.sh
  mv ../binDsk/binImg/*.tgz ../binImg/
  done
