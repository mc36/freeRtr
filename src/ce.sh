#!/bin/sh
echo compiling
ecj -source 8 -target 8 -Xlint:all -deprecation -d ../binOut/ -sourcepath ./ *.java
