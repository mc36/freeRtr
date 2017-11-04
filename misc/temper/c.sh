#!/bin/sh
./d.sh
javac -source 1.7 -target 1.7 -Xlint:all -deprecation *.java
