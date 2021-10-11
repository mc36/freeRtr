#!/bin/sh
echo compiling
ecj -source 11 -target 11 -Xlint:all -deprecation -d ../binOut/ -sourcepath ./ *.java net/freertr/*.java
