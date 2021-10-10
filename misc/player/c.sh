#!/bin/sh
./d.sh
javac -source 9 -target 9 -Xlint:all -deprecation *.java
