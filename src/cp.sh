#!/bin/sh
echo packing
TRG=../binOut/
#jar -c -0 -f rtr.jar -e org.freertr.router -C $TRG .
#jar -c -0 -f rtr.jar -m manifest.txt -C $TRG .
mkdir -p $TRG/META-INF
cp manifest.txt $TRG/META-INF/MANIFEST.MF
CWD=`pwd`
cd $TRG
LST=`find . | sort`
touch -d "2010-01-01 00:00:00" $LST
zip $CWD/rtr.jar $LST > /dev/null
cd $CWD
