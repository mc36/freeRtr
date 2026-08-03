#!/bin/sh
cd ../../src
java -Xmx1024m -jar rtr.jar test image ../misc/image/platform.amd64 ../misc/image/image.dsk
java -Xmx1024m -jar rtr.jar test image ../misc/image/platform.amd64 ../misc/image/image.gns
