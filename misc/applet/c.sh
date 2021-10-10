#!/bin/sh
./d.sh
javac -source 9 -target 9 -Xlint:all -deprecation -cp ../../src/rtr.jar *.java
zip ../../src/rtr.jar *.class
