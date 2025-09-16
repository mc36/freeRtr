#!/bin/sh
echo compiling
cp rtr.jar rtr2.jar
zip -d rtr2.jar "org/freertr/pipe/pipeWindow*"
native-image @native.txt
upx -o rtr2.bin rtr.bin
