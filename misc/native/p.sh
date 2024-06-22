#!/bin/sh
ARCH=-`uname -m`
echo `cd ../../binTmp/;tar cfz ../binImg/rtr$ARCH.tgz --owner=root --group=root *.bin *.so`
