#!/bin/sh
echo compiling
native-image --no-fallback --enable-all-security-services -jar rtr.jar rtr.bin
