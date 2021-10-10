#!/bin/sh
echo generating
find . -name "*.java" -print0 | xargs -s 512000 -0 javadoc -source 9 -use -linksource -keywords -quiet -d ../binTmp/
#doxygen ../misc/doxygen.cfg
