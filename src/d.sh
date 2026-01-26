#!/bin/sh
echo cleaning
rm -f *.jar 2> /dev/null
rm -f *.bin* 2> /dev/null
rm -f *.ver 2> /dev/null
rm -f *.tmp 2> /dev/null
rm -f *.pcap 2> /dev/null
rm -f *.mrt 2> /dev/null
rm -f *.log 2> /dev/null
rm -f rtr*-.csv 2> /dev/null
rm -f rtr*-.ftr 2> /dev/null
rm -f rtr*-.html 2> /dev/null
rm -f javac.*.args 2> /dev/null
rm -f ../changelog*-.txt 2> /dev/null
rm -f core 2> /dev/null
rm -f svm_err_*.md  2> /dev/null
rm -rf ../binTmp/* 2> /dev/null
rm -rf ../binOut/* 2> /dev/null
mkdir -p ../binTmp
mkdir -p ../binOut
mkdir -p ../binImg
