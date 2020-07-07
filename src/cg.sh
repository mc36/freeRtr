#!/bin/sh
echo compiling
zip -d rtr.jar pipe/pipeWindow*
native-image --allow-incomplete-classpath --no-fallback --enable-all-security-services -jar rtr.jar rtr.bin
