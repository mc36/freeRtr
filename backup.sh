#!/bin/sh
cd /rtr/src/
chmod -x `find . -executable -name *.java`
rm rtr.zip 2> /dev/null
echo -n `cd misc/android;./d.sh`
echo -n `cd misc/applet;./d.sh`
echo -n `cd misc/player;./d.sh`
echo -n `cd misc/temper;./d.sh`
echo -n `cd misc/trackmap;./d.sh`
echo -n `cd misc/voice;./d.sh`
echo `cd src;./d.sh`
zip -r rtr.zip src cfg misc *.sh *.txt>/dev/null
