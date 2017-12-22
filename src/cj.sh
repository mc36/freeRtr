#!/bin/sh
echo compiling
javac -source 7 -target 7 -Xlint:all -deprecation -d ../binTmp/ *.java
jar cfm0 rtr.jar manifest.txt -C ../binTmp/ .
