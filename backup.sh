#!/bin/sh
chmod -x `find . -executable -name *.java`
rm rtr.zip
echo `cd misc/trackmap;./d.sh`
echo `cd misc/player;./d.sh`
echo `cd misc/temper;./d.sh`
echo `cd src;./d.sh`
zip -r rtr.zip src cfg misc *.sh *.txt>/dev/null
