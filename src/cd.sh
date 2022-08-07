#!/bin/sh
echo scanning for duplicate tests
cat rtr.ftr | sort | uniq -d
echo scanning for non-ascii
find . -type f -exec grep -P "[^\x00-\x7F]" {} \;
echo scanning for trailing space
find . -type f -exec egrep -l " +$" {} \;
echo generating javadoc
find . -name "*.java" -print0 | xargs -s 512000 -0 javadoc -source 11 -use -linksource -keywords -quiet -d ../binTmp/
#doxygen ../misc/doxygen.cfg
