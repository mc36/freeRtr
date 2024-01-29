#!/bin/sh
ARCH=-`uname -m`
echo `cd ../../binTmp/;tar cf ../binImg/rtr$ARCH.tar --owner=root --group=root *.bin *.so`
