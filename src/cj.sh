#!/bin/sh
echo compiling
javac -source 9 -target 9 -Xlint:all -deprecation -d ../binOut/ net/freertr/*.java
