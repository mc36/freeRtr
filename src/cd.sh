#!/bin/sh
echo "scanning for duplicate features"
cat rtr.ftr | sort | uniq -d
echo "scanning for trailing space"
find . -type f -exec egrep -l " +$" {} \;
echo "scanning for non-ascii"
find . -type f -exec grep -P "[^\x00-\x7F]" {} \;
echo "scanning for stat-fin"
find . -type f -exec grep -P "static final" {} \;
echo "generating javadoc"
find . -name "*.java" -print0 | xargs -s 512000 -0 javadoc -source 11 -use -linksource -keywords -quiet -d ../binTmp/
#doxygen ../misc/doxygen.cfg
