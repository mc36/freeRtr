#!/bin/sh
ARCH=`uname -m`
echo packing for $ARCH
echo `cd ../../binTmp/;tar cfz ../binImg/rtr-$ARCH.tgz --owner=root --group=root *.bin *.so`
