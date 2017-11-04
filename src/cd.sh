#!/bin/sh
./d.sh
echo generating
javadoc -private -source 1.7 -use -linksource -keywords -quiet -d ../binTmp/ *
doxygen ../misc/doxygen.cfg
