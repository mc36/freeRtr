#!/bin/sh
for fn in *.tmpl ; do
  java optimizer $fn
  done
echo "##undef _TABLE_SIZE_P4_" > rare_profiles.p4
for fn in *.p4 ; do
  echo "#include \"$fn\"" >> rare_profiles.p4
  done
