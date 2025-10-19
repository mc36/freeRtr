#!/bin/sh
chmod -x `find . -executable -name "*.java"` 2> /dev/null
chmod -x `find . -executable -name "*.html"` 2> /dev/null
chmod -x `find . -executable -name "*.tmpl"` 2> /dev/null
chmod -x `find . -executable -name "*.tcl"` 2> /dev/null
chmod -x `find . -executable -name "*.tst"` 2> /dev/null
chmod -x `find . -executable -name "*.ini"` 2> /dev/null
chmod -x `find . -executable -name "*.txt"` 2> /dev/null
chmod -x `find . -executable -name "*.p4"` 2> /dev/null
chmod -x `find . -executable -name "*.py"` 2> /dev/null
chmod -x `find . -executable -name "*.c"` 2> /dev/null
chmod -x `find . -executable -name "*.h"` 2> /dev/null
echo -n `cd misc/trackmap;./d.sh`
echo -n `cd misc/position;./d.sh`
echo -n `cd misc/android;./d.sh`
echo -n `cd misc/gallery;./d.sh`
echo -n `cd misc/applet;./d.sh`
echo -n `cd misc/p4bmv2;./d.sh`
echo -n `cd misc/player;./d.sh`
echo -n `cd misc/temper;./d.sh`
echo -n `cd misc/mailer;./d.sh`
echo -n `cd misc/paster;./d.sh`
echo -n `cd misc/motion;./d.sh`
echo -n `cd misc/voice;./d.sh`
echo -n `cd misc/rfcs;./d.sh`
echo `cd src;./d.sh`
rm ipv*.mrt 2> /dev/null
rm rtr.zip 2> /dev/null
zip -r rtr.zip src cfg misc *.sh *.txt *.md >/dev/null
