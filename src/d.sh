#!/bin/sh
echo cleaning
rm rtr.jar
rm rtr.bin
rm rtr.ver
rm *.pcap
rm *.log
rm -Rf ../binTmp/*
mkdir -p ../binTmp
mkdir -p ../binDown
mkdir -p ../binImg
