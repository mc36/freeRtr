#!/bin/sh
./d.sh
javac -source 11 -target 11 -Xlint:all -deprecation *.java
