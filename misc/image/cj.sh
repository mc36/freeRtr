#!/bin/sh
cd ../../src
java -Xmx256m -jar rtr.jar test image ../misc/image/plat.amd64 ../misc/image/image.jvm
