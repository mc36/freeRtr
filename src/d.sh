#!/bin/sh
echo cleaning
rm -f rtr.jar 2> /dev/null
rm -f rtr.bin 2> /dev/null
rm -f rtr.ver 2> /dev/null
rm -f rtr*-.csv 2> /dev/null
rm -f rtr*-.ftr 2> /dev/null
rm -f rtr*-.html 2> /dev/null
rm -f ../changelog*-.txt 2> /dev/null
rm -f *.pcap 2> /dev/null
rm -f *.log 2> /dev/null
rm -Rf ../binTmp/* 2> /dev/null
rm -Rf ../binOut/* 2> /dev/null
mkdir -p ../binTmp
mkdir -p ../binOut
mkdir -p ../binDown
mkdir -p ../binImg
