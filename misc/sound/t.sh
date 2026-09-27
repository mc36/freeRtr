#!/bin/sh

. ../native/i.sh

for fn in tester1 tester2 tester3 tester4; do
  compileFile $fn "" "" ""
done

for fn in tester1 tester2 tester3 tester4; do
  ../../binTmp/$fn.bin
done
