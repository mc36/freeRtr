#!/bin/sh
fa=../../binTmp/fl1
fb=../../binTmp/fl2
ls -1 ../../binDwn/*.deb | sort | sed "s/\.\.\///" > $fa
cat ../../binDwn/*.lst | sort | uniq > $fb
wc -l $fa $fb
diff $fa $fb
