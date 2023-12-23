#!/bin/sh
cd ../../src
java -Xmx256m -jar rtr.jar test image ../misc/image/plat.amd64 ../misc/image/image.cmp
java -Xmx256m -jar rtr.jar test image ../misc/image/plat.i686 ../misc/image/image.cmp
java -Xmx256m -jar rtr.jar test image ../misc/image/plat.arm64 ../misc/image/image.cmp
java -Xmx256m -jar rtr.jar test image ../misc/image/plat.arm32 ../misc/image/image.cmp
java -Xmx256m -jar rtr.jar test image ../misc/image/plat.mips64 ../misc/image/image.cmp
java -Xmx256m -jar rtr.jar test image ../misc/image/plat.ppc64 ../misc/image/image.cmp
java -Xmx256m -jar rtr.jar test image ../misc/image/plat.risc64 ../misc/image/image.cmp
java -Xmx256m -jar rtr.jar test image ../misc/image/plat.s390x ../misc/image/image.cmp
java -Xmx256m -jar rtr.jar test image ../misc/image/plat.sparc ../misc/image/image.cmp
