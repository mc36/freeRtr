#!/bin/sh
getRtr()
{
cp ../../binTmp/zzz0$1-hw.txt ./$1-hw.txt
cp ../../binTmp/zzz0$1-sw.txt ./$1-sw.txt
}
getRtr r1
getRtr r2
getRtr r3
getRtr r4
