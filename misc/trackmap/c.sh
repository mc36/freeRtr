#!/bin/sh
./d.sh
dia -e home.png home.dia
javac -source 11 -target 11 -Xlint:all -deprecation *.java
