#!/bin/sh
echo compiling
javac -source 8 -target 8 -Xlint:all -deprecation -d ../binOut/ *.java
