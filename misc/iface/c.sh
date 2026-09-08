#!/bin/sh

. ../native/i.sh

for fn in pcapInt; do
  compileFile $fn "" "-lpthread -lpcap" ""
done

for fn in xskInt; do
  compileFile $fn "" "-lpthread -lxdp" ""
done

for fn in urngInt; do
  compileFile $fn "" "-lpthread -luring" ""
done

for fn in mapInt cmp1int cmp2int rawInt tapInt fileInt; do
  compileFile $fn "" "-lpthread" ""
done
