#/bin/sh
for fn in *.txt ; do
    java -jar ../../src/rtr.jar test yangconfig $fn ../../binTmp/
    done
rm reload.log
