#!/bin/sh
./d.sh
echo compiling
javac -source 1.7 -target 1.7 -Xlint:all -deprecation -d ../binTmp/ *.java
jar cfm0 rtr.jar manifest.txt -C ../binTmp/ .
java -Xmx256m -jar rtr.jar test verfile rtr.key rtr.zip@../rtr.zip
java -Xmx256m -jar rtr.jar test vercore rtr.key
java -Xmx256m -jar rtr.jar exec flash verify
