#!/bin/sh
ARCH=`uname -m`
echo packing for $ARCH
echo `cd ../../binTmp/;tar cfz ../binImg/utl-$ARCH.tgz --owner=root --group=root *.bin`
