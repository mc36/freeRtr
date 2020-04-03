#!/bin/sh
./d.sh
echo generating
find . -name "*.java" -print0 | xargs -s 512000 -0 javadoc -private -source 8 -use -linksource -keywords -quiet -d ../binTmp/
#doxygen ../misc/doxygen.cfg
