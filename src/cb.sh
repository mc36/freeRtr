#!/bin/sh
echo compiling
cp rtr.jar rtr2.jar
zip -d rtr2.jar "net/freertr/pipe/pipeWindow*"
native-image --no-fallback -jar rtr2.jar rtr.bin
