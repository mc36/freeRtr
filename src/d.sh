#!/bin/sh
echo cleaning
rm rtr.jar 2> /dev/null
rm rtr.bin 2> /dev/null
rm rtr.ver 2> /dev/null
rm *.pcap 2> /dev/null
rm *.log 2> /dev/null
rm -Rf ../binTmp/* 2> /dev/null
mkdir -p ../binTmp
mkdir -p ../binDown
mkdir -p ../binImg
