#!/bin/sh
echo compiling
cp rtr.jar rtr2.jar
zip -d rtr2.jar "pipe/pipeWindow*"
native-image --no-server --allow-incomplete-classpath --no-fallback --enable-all-security-services -jar rtr2.jar rtr.bin
