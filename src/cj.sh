#!/bin/sh
echo compiling
javac -source 1.7 -target 1.7 -Xlint:all -deprecation -d ../binTmp/ *.java
jar cfm0 rtr.jar manifest.txt -C ../binTmp/ .
