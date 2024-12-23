#!/bin/sh
for MSG in `ls ../binTmp/*.err 2> /dev/null` ; do
  cat $MSG | s-nail -s "tester errors happened" freerror@nop.hu
  done
