#!/bin/sh
echo compiling
ecj -source 8 -target 8 -Xlint:all -deprecation -d ../binTmp/ -sourcepath ./ *.java
jar cfm0 rtr.jar manifest.txt -C ../binTmp/ .
