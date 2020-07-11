#!/bin/sh
echo packing
TRG=../binOut/
#jar cfm0 rtr.jar manifest.txt -C $TRG .
mkdir -p $TRG/META-INF
cp manifest.txt $TRG/META-INF/MANIFEST.MF
CWD=`pwd`
cd $TRG
LST=`find . | sort`
touch -d "2010-01-01 00:00:00" $LST
zip -0 $CWD/rtr.jar $LST > /dev/null
cd $CWD
