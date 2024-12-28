#!/bin/sh
./cn.sh
ulimit -n 16384
export p4emuNOCMDS=1
export p4emuNOCONS=1
java -Xmx512m -jar rtr.jar test tester p4lang- binary other p4lang7.ini slot 1 parallel 30 retry 4 $@
./te.sh
