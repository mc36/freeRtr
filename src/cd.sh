#!/bin/sh
echo scanning
grep -P -n "[\x80-\xFF]" `find .` 2> /dev/null
echo generating
find . -name "*.java" -print0 | xargs -s 512000 -0 javadoc -source 11 -use -linksource -keywords -quiet -d ../binTmp/
#doxygen ../misc/doxygen.cfg
