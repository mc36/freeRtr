#!/bin/sh
echo compiling
javac -source 8 -target 8 -Xlint:all -deprecation -d ../binTmp/ *.java
jar cfm0 rtr.jar manifest.txt -C ../binTmp/ .
