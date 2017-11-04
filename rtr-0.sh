#!/bin/sh
mkdir -p rtr
rm -rf rtr/src/*
rm -rf rtr/cfg/*
rm -rf rtr/misc/*
wget -Ortr/rtr.zip http://safe.mchome.nop.hu/rtr.zip
unzip -o rtr/rtr.zip -d rtr/
chmod 777 rtr/src/*.sh
chmod 777 rtr/misc/*.sh
echo `cd rtr/src;./c.sh`
echo `cd rtr/misc/native;./c.sh`
