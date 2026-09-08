#!/bin/sh

. ../native/i.sh

for fn in bundle vlan; do
  compileFile $fn "" "-lpthread" ""
done

for fn in veth; do
  compileFile $fn "" "-lmnl" ""
done

for fn in ptyRun; do
  compileFile $fn "" "-lutil" ""
done

for fn in seth rexec daemonRun dirRun; do
  compileFile $fn "" "" ""
done

for fn in connect flood; do
  compileFile $fn "" "-lpthread -lpcap" ""
done
