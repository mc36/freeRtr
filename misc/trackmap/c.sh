#!/bin/sh
./d.sh
dia -e home.png home.dia
javac -source 9 -target 9 -Xlint:all -deprecation *.java
