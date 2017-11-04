cp ../../binTmp/r1-?w.txt ./
cp ../../binTmp/r2-?w.txt ./
cp ../../binTmp/r3-?w.txt ./
cp ../../binTmp/r4-?w.txt ./
cp ../../binTmp/r5-?w.txt ./
cat get.txt >>r1-sw.txt
cat get.txt >>r2-sw.txt
cat get.txt >>r3-sw.txt
cat get.txt >>r4-sw.txt
cat get.txt >>r5-sw.txt
echo tcp2vrf 2001 v1 23 >>r1-hw.txt
echo tcp2vrf 2002 v1 23 >>r2-hw.txt
echo tcp2vrf 2003 v1 23 >>r3-hw.txt
echo tcp2vrf 2004 v1 23 >>r4-hw.txt
echo tcp2vrf 2005 v1 23 >>r5-hw.txt
