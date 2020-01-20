#!/bin/sh
./d.sh
dia -e home.png home.dia
javac -source 8 -target 8 -Xlint:all -deprecation *.java
