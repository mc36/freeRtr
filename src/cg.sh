#!/bin/sh
./d.sh
echo compiling
gcj-wrapper -Xlint:all -deprecation -O3 -d ../binTmp/ router.java
echo linking
gcj --main=router -O3 -o rtr.bin `find ../binTmp/ | grep "\.class$"`
