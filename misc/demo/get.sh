#!/bin/sh

cd ../../src
./tw.sh demo02 oobase 2001
cd ../misc/demo

getRtr()
{
cp ../../binTmp/zzz0$1-hw.txt ./$1-hw.txt
cp ../../binTmp/zzz0$1-sw.txt ./$1-sw.txt
}

getRtr r1
getRtr r2
getRtr r3
getRtr r4
