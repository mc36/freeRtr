#!/bin/sh
./d.sh
echo compiling
ecj -7 -deprecation -d ../binTmp/ router.java
jar cfm rtr.jar manifest.txt -C ../binTmp/ .
