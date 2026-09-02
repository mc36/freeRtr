#!/bin/sh
echo compiling
cp rtr.jar rtr2.jar
zip -d rtr2.jar "org/freertr/pipe/pipeWindow*"
native-image @native.txt
cp rtr.bin ../binImg/rtr-`uname -m`.bin
