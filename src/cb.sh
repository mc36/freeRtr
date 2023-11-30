#!/bin/sh
echo compiling
cp rtr.jar rtr2.jar
zip -d rtr2.jar "net/freertr/pipe/pipeWindow*"
zip -d rtr2.jar "net/freertr/user/userGame**"
native-image -O3 --no-fallback -jar rtr2.jar -o rtr.bin
upx -o rtr2.bin rtr.bin
