#!/bin/sh
cd ../misc/native
./c.sh
cd ../../src
./c.sh
./cb.sh
java -Xmx512m -jar rtr.jar test tester p4lang- binary other p4lang9.ini summary slot 1 paralell 60 retry 4 url http://sources.freertr.org/cfg/ $@
java -Xmx512m -jar rtr.jar test tstsum
./te.sh
