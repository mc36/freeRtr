#!/bin/sh
java -Xmx512m -jar rtr.jar test tester opnflw- other opnflw.ini summary slot 121 retry 8 url http://sources.freertr.org/cfg/ $@
./te.sh
for a in csv ftr html ; do
  echo updating $a offload
  cp rtropnflw-openflow-.$a rtr3.$a
  done
