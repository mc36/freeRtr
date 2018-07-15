#!/bin/sh
./d.sh
echo generating
javadoc -private -source 1.7 -use -linksource -keywords -quiet -d ../binTmp/ router.java
#doxygen ../misc/doxygen.cfg
###cd .. ; find src -name *.java -and -type f -print0 | xargs -s 512000 -0 /usr/lib/jvm/java-10-openjdk-amd64/bin/javadoc -quiet -d binTmp/ -source 1.7
