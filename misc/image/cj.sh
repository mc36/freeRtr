#!/bin/sh
cd ../../src
java -Xmx256m -jar rtr.jar test image ../misc/image/image.clr
java -Xmx256m -jar rtr.jar test image ../misc/image/plat.amd64 ../misc/image/image.jvm
java -Xmx256m -jar rtr.jar test image ../misc/image/plat.i686 ../misc/image/image.jvm
java -Xmx256m -jar rtr.jar test image ../misc/image/plat.arm64 ../misc/image/image.jvm
