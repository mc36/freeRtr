#!/bin/sh
./d.sh
dia -e home.png home.dia
javac -source 1.7 -target 1.7 -Xlint:all -deprecation *.java
