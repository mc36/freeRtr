#/bin/sh
for fn in *.txt ; do
    java -jar ../../src/rtr.jar test yangsensor $fn ../../binTmp/
    done
rm reload.log
